:- module(comp, [
	compile_terms/3,
	clauses_ir/2,
	goal_ir/2,
	terms_ir/2
]).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(readutil), [read_file_to_terms/3]).
:- use_module(library(gensym), [gensym/2]).
:- use_module(optimizations).
:- use_module(directives).
:- use_module(helpers, [args_list/2]).
:- use_module(ir, [walk_ir/3]).
:- use_module(goal_ir).
:- use_module(variables).

% clauses_grouped groups a list of clauses (H :- B terms) into sublists
% grouped by head name/arity.
clauses_grouped([], []).
clauses_grouped([X], [[X]]).
clauses_grouped(Cs, [G|Gs]) :-
	% Find longest prefix of matching clauses.
	once((
		append(G, Suffix, Cs),
		( Suffix = []
		; clauses_grouped__first_spec_(G, A0/N0),
			clauses_grouped__first_spec_(Suffix, A1/N1),
		  A0/N0 \= A1/N1
		)
	)),
	clauses_grouped(Suffix, Gs).

clauses_grouped__first_spec_([(:- _)|_], (:-)/1).
clauses_grouped__first_spec_([(H :- _)|_], A/N) :- functor(H, A, N).

% terms_ir(Terms, IR) where Terms is like the output of
% read_file_to_terms/3.
terms_ir -->
	maplist(normalize_clause),
	clauses_grouped,
	maplist(terms_ir__tform_).

terms_ir__tform_(X, IR) :-
	clauses_ir(X, IR) *-> true
	; format(string(Message), "Failed to compile clauses: ~w", [X]),
	domain_error(rule_or_directive, Message).

% Compiles a list of Prolog clauses to an IR that can be passed to a backend
% clauses_ir(+Clauses, -IR)
clauses_ir([], nothing).
clauses_ir(Clauses, IR) :-
	Clauses = [(:-_)|_],
	maplist(arg(1), Clauses, Directives),
	maplist(directive_ir, Directives, IRList),
	args_list(IR, IRList).

clauses_ir(Clauses, (
	$.(FnName) := fn(generator, (
		$.("CALLED_TERM") := Callee,
		AllocVars,
		Impls
	)),
	db_set(Module, Name, Arity, $.(FnName))
)) :-
	% Clauses are facts or rules, not directives.
	( Clauses = [(_:-_)|_] -> true ; Clauses \= [(:- _)|_] ),

	% Set up local variables to add to the generated closure.
	% TODO: Re-use variables after backtracking instead of generating
	% 	separately for each clause -- OR allocate at clause start instead
  numbervars_allocation(Clauses, AllocVars),

	% TODO utility function for this -- i think it's repeated a
	% couple times
	Clauses = [FirstClause|_],
	normalize_clause(FirstClause, (NormHead :- _)),
	functor(NormHead, Name, Arity),

	memberchk(Arity-Callee, [
		0- \Name,
		_-make_term(\Name, arguments)
	]),


	% Build the IR
	compile_current_module(Module),
	normalize_and_compile_clauses(Clauses, Impls).

normalize_and_compile_clauses([], nothing).
normalize_and_compile_clauses([Clause|Rest], (ClauseIR, RestIR)) :-
	normalize_clause(Clause, Norm),
	compile_clause(Norm, ClauseIR),
	normalize_and_compile_clauses(Rest, RestIR).

compile_clause((Head :- Body), (
	funcall(user, "unify", [$.("CALLED_TERM"), \Head]) *-> BodyIR
)) :-	goal_ir(Body, BodyIR).

var_name_(N, $.(Name)) :- format(string(Name), "~d", [N]).


% Convert a clause to Head :- Body format
normalize_clause((:- B), (:- B)) :- !.
normalize_clause((H :- B), (H :- B)) :- !.
normalize_clause(Fact, (Fact :- true)) :- !.

% Helper: identity transformation for already compiled strings.
compile_node(_, N, N) :- string(N), !.
compile_node(Backend, N, Out) :-
    call(Backend, N, Out)
		*-> true
		; throw(backend_failed(Backend, N)).

:- meta_predicate compile_term(2, +, -).
compile_term_ir(Backend) -->
  walk_ir([I, I] >> (
		member(I, [$X, $.(X), $(X, _)]),
		ensure_sym(X)
	)),
	walk_ir(erase_true),
	walk_ir(loop_to_yield_all),
	walk_ir(compile_node(Backend)).

compile_terms(Backend, Terms, Out) :-
	terms_ir(Terms, IRs),
	maplist(compile_term_ir(Backend), [file_start|IRs], OutCodes),
	maplist(codes_to_string, OutCodes, Outs),
	atomics_to_string(Outs, Out).

% Helper to convert difference list codes to a string
codes_to_string(Codes, String) :-
	phrase(Codes, CodesList),
	string_codes(String, CodesList).

compile_file(Backend, FName, Out) :-
	%% NOTE: This may be a self-referencing import. But apparently,
	%% that's no problem.
	Terms=[(:- ensure_loaded(library("prelude")))|FileTerms],
	read_file_to_terms(FName, FileTerms, []),
	compile_terms(Backend, Terms, Out).

ensure_sym(S) :- nonvar(S), ! ; gensym("gen", A), atom_string(A, S).

:- begin_tests(comp).
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
