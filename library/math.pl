:- ensure_loaded(library(native/math)).
:- ensure_loaded(library(native/types)).

X is Expr :- var(Expr), throw("is/2 expression must be ground").
N is N :- number(N), !.
X is Expr :-
	Expr =.. [Op, EA, EB],
	A is EA,
	B is EB,
	'$is_op'(Op, X, A, B).

plus(A, B, C) :- nonvar(A), nonvar(B), !,
	C is A + B.

plus(A, B, C) :- nonvar(A), nonvar(C), !,
	B is C - A.

plus(A, B, C) :- nonvar(B), nonvar(C), !,
	A is C - B.
