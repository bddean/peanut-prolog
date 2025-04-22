:- module(comp, [
    compile_clauses/2,
    compile_goal/2,
    term_to_call/2,
    compile_with_backend/3
]).
:- use_module(library(debug), [assertion/1]).

:- meta_predicate walk_ir(2, ?, ?).
walk_ir(G) --> walk_kids(G), tform_node(G).

tform_node(G, E0, E) :- call(G, E0, E) *-> true ; E = E0.

walk_kids(G, E0, E) :- walk_kids_(G, E0, E) *-> true ; E = E0.
walk_kids_(G, E0, E) :-
	member(E0-E, [
		(M0, N0)    - (M, N),
		(M0 -> N0)  - (M -> N),
		(M0 *-> N0) - (M *-> N)
	]),
	maplist(walk_ir(G), [M0, N0], [M, N]).

% Compiles a list of Prolog clauses to an IR that can be passed to a backend
% compile_clauses(+Clauses, -IR)
% TODO: Split this into two steps:
% 1. Convert the clauses to a single disjunction, including unification to each head...
% 2. Then actually compile to the IR!
compile_clauses([], nothing).
compile_clauses(Clauses, defun(generator, Spec, "X", Impls)) :-
	Clauses = [FirstClause|_],
	normalize_clause(FirstClause, NormHead, _),
	functor(NormHead, Name, Arity),
	% Check that all clauses are for the same predicate
	forall(member(C, Clauses), (
	    normalize_clause(C, H, _),
	    functor(H, Name, Arity)
	)),
	% Build the IR
	Spec = Name/Arity,
	normalize_and_compile_clauses(Clauses, Impls).

% Normalize and compile clauses
normalize_and_compile_clauses([], nothing).
normalize_and_compile_clauses([Clause|Rest], (ClauseIR ; RestIR)) :-
	normalize_clause(Clause, Head, Body),
	compile_clause(Head, Body, ClauseIR),
	normalize_and_compile_clauses(Rest, RestIR).

% Compile a single clause
compile_clause(Head, Body, IR) :-
	copy_term((Head, Body), (HeadCopy, BodyCopy)),
	numbervars((HeadCopy, BodyCopy), 0, _),
	compile_goal(BodyCopy, BodyIR),
	IR = (funcall("unify", [$"X", \HeadCopy]) *-> BodyIR).

% Convert a clause to Head :- Body format
normalize_clause((H :- B), H, B) :- !.
normalize_clause(Fact, Fact, true) :- !.

% Convert Prolog statements to IR
% compile_goal(+Statement, -IR)
compile_goal(G, IR) :- compile_goal(yield, G, IR).

compile_goal(Cont, (A0, B0), A) :-
	!,
	compile_goal(Cont, B0, B),
	compile_goal(B, A0, A).

compile_goal(Cont, (A0 ; B0), (A , B)) :- % TODO names etc
	!,
	maplist(compile_goal(Cont), [A0, B0], [A, B]).

compile_goal(_, !, break) :- !.

% TODO convert to -> in second pass
compile_goal(Cont, Term, (Call *-> Cont)) :-
	term_to_call(Term, Call).

% Convert a Prolog term to a function call
% term_to_call(+Term, -Call)
term_to_call(Term, funcall(Name, Args)) :-
	Term =.. [Name|Args0],
	wrap_args(Args0, Args).

% Wrap arguments in the IR format
% wrap_args(+PrologArgs, -WrappedArgs)
wrap_args(Ts, As) :- maplist(wrap_args__escape_, Ts, As).
wrap_args__escape_(X, \X).

% TODO: Comment too verbose.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile_with_backend/3
%%
%% compile_with_backend(+BackendPred, +Clauses:list, -Output:string)
%%   BackendPred is a predicate of arity‑2 that will be called as
%%        call(BackendPred, Node, OutString)
%%   where Node is an IR node whose *direct* children have already been
%%   compiled to strings.  BackendPred must succeed deterministically and
%%   bind OutString to a string representing the generated target code for
%%   that Node.
%%
%% Example use:
%%     comp:compile_with_backend(js2:emit, Clauses, JS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helper: identity transformation for already compiled strings.
compile_node(_, N, N) :- string(N), !.
compile_node(Backend, N, Out) :-
    call(Backend, N, Out)
		*-> true
		; throw(backend_failed(Backend, N), "Backend failed").

compile_with_backend(Backend, Clauses, IR) :-
    compile_clauses(Clauses, IR0),
    phrase(walk_ir(compile_node(Backend)), IR0, IR).

:- begin_tests(comp).
test(compile_empty, nondet) :-
	compile_clauses([], IR),
	IR == nothing.

test(compile_simple_clause, nondet) :-
	compile_clauses([foo(X) :- bar(X)], IR),
	IR = defun(generator, foo/1, "X", _Body).

test(statement_conjunction, nondet) :-
	compile_goal((a(1), b(2)), IR),
	assertion( IR = (StmtA *-> StmtB *-> yield) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(statement_disjunction, nondet) :-
	compile_goal((a(1) ; b(2)), IR),
	assertion( IR = (StmtA , StmtB) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(statement_cut) :-
	compile_goal(!, IR),
	IR == break.

test(term_to_call) :-
	term_to_call(writeln(hello), IR),
	IR = funcall(writeln, [_]).

test(compile_facts) :-
	compile_clauses([foo(1), foo(2), foo(3)], IR),
	IR = defun(generator, foo/1, "X", _Body).

test(compile_multiple_predicates, fail) :-
	% This should fail as we don't yet support multiple predicates in one compile
	compile_clauses([foo(1), bar(2)], _).

test(complex_predicate) :-
	Clauses = [
	    (foo(X, Y) :- bar(X), baz(Y)),
	    (foo(X, Y) :- qux(X, Y))
	],
	compile_clauses(Clauses, IR),
	IR = defun(generator, foo/2, "X", _Impl).

:- end_tests(comp).
