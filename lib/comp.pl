decls([], nothing).
decls(
	[(Head :- Body)|Rest],
	defun(
		generator,
		$Spec, %% TODO find way to make this functional syntax work...
		"X",
		Impls
	)
) :-
	spec(Head, Spec),
	%% NOTE: Pass backend as ctx via dynamic database...
	decls_body(Body, Impls).

decls_body([], nothing).
decls_body(
	[(Head :- Body)|Rest],
	Impl ; Impls
) :-
	Impl = (
		%% TODO: Optimize this by skipping unnecessary head unifications.
		funcall("unify", [\Head, "X"]) *->
		statement(Body)
	),
	decls_body(Rest, Impls).

statement((F , G), Expr) :-
	term_as_funcall(F, S),
	statement(G, T),
	is_predicate(F) -> Expr = (S -> T) ; Expr = (S *-> T).

statement((F ; G), (S; T)) :-
	statement(F, S), statement(G, T).
statement(!, !).

term_as_funcall(T, funcall($Spec, \Args)) :- args(T, Args).

%% Utils:
args(T, As) :- atomic(T) -> As = [] ; T =.. [_|As].
spec(T, Name/Arity) :-
	atomic(T) -> Name/Arity = T/0
	; functor(T, Name, Arity).

is_predicate(_) :- fail. %% Not implemented yet.
