member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

memberchk(X, L) :- member(X, L), !.

append([], L, L).
append([H|T], L, [H|Result]) :- append(T, L, Result).

