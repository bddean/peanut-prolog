:- module(comp, [
		% TODO -- this should be the main entry point!
		compile_terms/3,

    clauses_ir/2,
    goal_ir/2,
    term_call_ir/2,
		terms_ir/2
]).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(readutil), [read_file_to_terms/3]).
:- use_module(library(gensym), [gensym/2, reset_gensym/0]).
:- use_module('./optimizations').

:- meta_predicate walk_ir(2, ?, ?).
walk_ir(G) --> walk_kids(G), tform_node(G).
tform_node(G, E0, E) :- call(G, E0, E) *-> true ; E = E0.

walk_kids(G, E0, E) :- walk_kids_(G, E0, E) *-> true ; E = E0.
walk_kids_(G, E0, E) :-
	member(E0-E, [
		(M0, N0)    - (M, N),
		(M0 -> N0)  - (M -> N),
		(M0 *-> N0) - (M *-> N),
		(M0 := N0) - (M := N)
	]),
	maplist(walk_ir(G), [M0, N0], [M, N]).

walk_kids_(G, yield_all(X0), yield_all(X)) :- walk_ir(G, X0, X).

walk_kids_(
	G,
	defun(Type, Name, Body0),
	defun(Type, Name, Body)
) :-
	walk_ir(G, Body0, Body).

walk_kids_(
	G,
	funcall(Name, Args0),
	funcall(Name, Args)
) :-
	maplist(walk_ir(G), Args0, Args).

walk_kids_(
	G,
	allocate_vars(Names0),
	allocate_vars(Names)
) :-
	maplist(walk_ir(G), Names0, Names).

walk_kids_(G, L0:B0, L:B) :-	maplist(walk_ir(G), [L0, B0], [L, B]).
walk_kids_(G, break(L0), break(L)) :- call(G, L0, L).

% clauses_grouped groups a list of clauses (H :- B terms) into sublists
% grouped by head name/arity.
clauses_grouped([], []).
clauses_grouped([X], [[X]]).
clauses_grouped(Cs, [G|Gs]) :-
	% Find longest prefix of matching clauses.
	once((
		append(G, Suffix, Cs),
		( Suffix = []
		; G=[(H0 :- _)|_],
		  Suffix=[(H1 :- _)|_],
		  functor(H0, A0, N0),
		  functor(H1, A1, N1),
		  A0/N0 \= A1/N1
		)
	)),
	clauses_grouped(Suffix, Gs).

% terms_ir(Terms, IR) where Terms is like the output of
% read_file_to_terms/3.
terms_ir -->
	maplist(normalize_clause),
	clauses_grouped,
	maplist(terms_ir__tform_).

terms_ir__tform_(X, IR) :-
	clauses_ir(X, IR) *-> true
	; declaration_ir(X, IR).

declaration_ir(_) :- throw("Declarations not supported yet").

% Compiles a list of Prolog clauses to an IR that can be passed to a backend
% clauses_ir(+Clauses, -IR)
% TODO: Split this into two steps:
% 1. Convert the clauses to a single disjunction, including unification to each head...
% 2. Then actually compile to the IR!
clauses_ir([], nothing).
clauses_ir(Clauses, defun(generator, Spec, (
	allocate_vars(VarNames),
	Impls
))) :-
	% Set up local variables to add to the generated closure.
	% TODO: Re-use variables after backtracking instead of generating
	% 	separately for each clause -- OR allocate at clause start instead
	numbervars(Clauses, 0, NumVars),
	numlist(0, NumVars, VarNums),
	maplist(var_name_, VarNums, VarNames),

	% TODO utility function for this -- i think it's repeated a
	% couple times
	Clauses = [FirstClause|_],
	normalize_clause(FirstClause, (NormHead :- _)),
	functor(NormHead, Name, Arity),

	% Build the IR
	Spec = Name/Arity,
	normalize_and_compile_clauses(Clauses, Impls).

normalize_and_compile_clauses([], nothing).
normalize_and_compile_clauses([Clause|Rest], (ClauseIR, RestIR)) :-
	normalize_clause(Clause, Norm),
	compile_clause(Norm, ClauseIR),
	normalize_and_compile_clauses(Rest, RestIR).

compile_clause((Head :- Body), (
	funcall("unify", [$.("CALLED_TERM"), \Head]) *-> BodyIR
)) :-	goal_ir(Body, BodyIR).

var_name_(N, $.(Name)) :- format(string(Name), "~d", [N]).
arglist([], nothing).
arglist([X], X).
arglist([X|Xs], (X, As)) :- arglist(Xs, As).

% Convert a clause to Head :- Body format
normalize_clause((H :- B), (H :- B)) :- !.
normalize_clause(Fact, (Fact :- true)) :- !.

% Convert Prolog statements to IR
% goal_ir(+Statement, -IR)
goal_ir(G, IR) :- goal_ir(function, yield, G, IR).

goal_ir(Scope, Cont, (A0, B0), A) :-
	!,
	goal_ir(Scope, Cont, B0, B),
	goal_ir(Scope, B, A0, A).

goal_ir(Scope, Cont, (A0 ; B0), (A , B)) :-
	!,
	maplist(
		goal_ir(Scope, Cont),
		[A0, B0],
		[A, B]
	).

goal_ir(function, Cont, !, (Cont, return)) :- !.
goal_ir(block(Lbl), Cont, !, (Cont, break($Lbl))) :- !.

% TODO convert to -> in second pass
goal_ir(_, Cont, Term, (Call *-> Cont)) :-
	term_call_ir(Term, Call).

% Convert a Prolog term to a function call
% term_call_ir(+Term, -Call)
term_call_ir(Term, funcall(Name, Args)) :-
	Term =.. [Name|Args0],
	wrap_args(Args0, Args).

% Wrap arguments in the IR format
% wrap_args(+PrologArgs, -WrappedArgs)
wrap_args(Ts, As) :- maplist(wrap_args__escape_, Ts, As).
wrap_args__escape_(X, \X).

% Helper: identity transformation for already compiled strings.
compile_node(_, N, N) :- string(N), !.
compile_node(Backend, N, Out) :-
    call(Backend, N, Out)
		*-> true
		; throw(backend_failed(Backend, N)).

:- meta_predicate compile_term(2, +, -).
compile_term_ir(Backend) -->
	walk_ir(erase_true),
	walk_ir(loop_to_yield_all),
	walk_ir(compile_node(Backend)).

compile_terms(Backend, Terms, Out) :-
  terms_ir(Terms, IRs),
	maplist(compile_term_ir(Backend), IRs, OutCodes),
	maplist(codes_to_string, OutCodes, Outs),
	atomics_to_string(Outs, Out).

% Helper to convert difference list codes to a string
codes_to_string(Codes, String) :-
	phrase(Codes, CodesList),
	string_codes(String, CodesList).

compile_file(Backend, FName, Out) :-
	% TODO do htis cleaner-ly.
	read_file_to_terms("./lib/prelude.pl", Terms, [tail(FileTerms)]),
	read_file_to_terms(FName, FileTerms, []),
	compile_terms(Backend, Terms, Out).

ensure_sym(S) :- nonvar(S), ! ; gensym("gen", A), atom_string(A, S).

:- begin_tests(comp).
test(compile_empty, nondet) :-
	clauses_ir([], IR),
	IR == nothing.

test(compile_simple_clause, nondet) :-
	clauses_ir([foo(X) :- bar(X)], IR),
	assertion(IR = defun(generator, foo/1, _Body)).

test(statement_conjunction, nondet) :-
	goal_ir((a(1), b(2)), IR),
	assertion( IR = (StmtA *-> StmtB *-> yield) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(statement_disjunction, nondet) :-
	goal_ir((a(1) ; b(2)), IR),
	assertion( IR = (StmtA , StmtB) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(term_call_ir) :-
	term_call_ir(writeln(hello), IR),
	IR = funcall(writeln, [_]).

test(compile_facts, [nondet]) :-
	clauses_ir([foo(1), foo(2), foo(3)], IR),
	IR = defun(generator, foo/1, _Body).

test(complex_predicate, nondet) :-
	Clauses = [
	    (foo(X, Y) :- bar(X), baz(Y)),
	    (foo(X, Y) :- qux(X, Y))
	],
	clauses_ir(Clauses, IR),
	IR = defun(generator, foo/2, _).

test(clauses_grouped) :-
	Clauses = [
		(foo(X) :- bar(X)),
		(foo(Y) :- baz(Y)),
		(bar(Z) :- qux(Z)),
		(bar(W) :- quux(W)),
		(abc(A) :- def(A))
	],
	clauses_grouped(Clauses, Groups),
	!,
	assertion(length(Groups, 3)),
	assertion(Groups = [
		[(foo(X) :- bar(X)), (foo(Y) :- baz(Y))],
		[(bar(Z) :- qux(Z)), (bar(W) :- quux(W))],
		[(abc(A) :- def(A))]
	]).

test(member_clauses_grouped) :-
	Clauses = [
		(member(X, [X]) :- true),
		(member(X, [_|L]) :- member(X, L))
	],
	clauses_grouped(Clauses, Groups),
	!,
	assertion(length(Groups, 1)),
	assertion(Groups = [
		[(member(X, [X]) :- true), (member(X, [_|L]) :- member(X, L))]
	]).

:- end_tests(comp).
