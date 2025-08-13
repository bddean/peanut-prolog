:- ensure_loaded(library(native/math)).
:- ensure_loaded(library(native/types)).

is(X, Expr) :- var(Expr), throw("is/2 expression must be ground").
is(N, N) :- number(N), !.
is(X, Expr) :-
	Expr =.. [Op, EA, EB],
	A is EA,
	B is EB,
	'$is_op'(Op, X, A, B).
