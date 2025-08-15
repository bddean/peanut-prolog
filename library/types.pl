:- ensure_loaded(library(native/types)).

atomic(A) :-
	atom(A),
	! ; string(A),
	! ; number(A).
