:- ensure_loaded(library(native/arrays)).

array_list(Arr, Ls) :- nonvar(Arr), !,
	array_length(Arr, N),
	array_to_list_(Arr, Ls, 0, N).

array_list(Arr, Ls) :-
	Arr = #(0), %% TODO support #()
	maplist(array_push(Arr), Ls),
	array_shift(Arr, _).

array_to_list_(Arr, [], N, N) :- !.
array_to_list_(Arr, [H|T], I, N) :-
	array_at(Arr, I, H),
	In is I + 1,
	array_to_list_(Arr, T, In, N).
