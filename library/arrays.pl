:- ensure_loaded(library(native/arrays)).

array_list(Arr, Ls) :- nonvar(Arr), !,
	array_length(Arr, N),
	array_to_list_(Arr, Ls, 0, N).

array_list(Arr, Ls) :-
	Arr = #(0), %% TODO support #(). This is a hack.
	array_from_list_(Arr, Ls),
	array_shift(Arr, _).

%% Can't use maplist since it uses =../2, which makes
%% it an infinite recursion.
array_from_list_(Arr, []) :- !.
array_from_list_(Arr, [H|T]) :-
	array_push(Arr, H),
	array_from_list_(Arr, T).


array_to_list_(Arr, [], N, N) :- !.
array_to_list_(Arr, [H|T], I, N) :-
	array_at(Arr, I, H),
	succ(I, In), %% is/2 here results in infinite recursion!
	array_to_list_(Arr, T, In, N).
