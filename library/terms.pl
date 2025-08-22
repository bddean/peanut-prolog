:- ensure_loaded(library(native/terms)).
:- ensure_loaded(library(arrays)).
:- ensure_loaded(library(types)).

T =.. [A|Xs] :-
	nonvar(T), !,
		term_tag_args(T, A, Xz),
		array_list(Xz, Xs)
	; array_list(Xz, Xs),
		term_tag_args(T, A, Xz).
