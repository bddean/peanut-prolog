:- ensure_loaded(library(math)).

between(Low, High, X) :-
	nonvar(X),
	!,
	Low =< X, X =< High.
between(X, High, X) :- X =< High.
between(Low, High, X) :-
	Low < High,
	succ(Low, LowN),
	between(LowN, High, X).

