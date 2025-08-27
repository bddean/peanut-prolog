:- ensure_loaded(library(native/types)).

atomic(A) :-
	atom(A),
	! ; string(A),
	! ; number(A).

is_list(V) :- var(V), !, fail.
is_list([]).
is_list([_|L]) :- is_list(L).
