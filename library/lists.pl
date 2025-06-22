member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

memberchk(X, L) :- member(X, L), !.

append([], L, L).
append([H|T], L, [H|Result]) :- append(T, L, Result).

append([], []).
append([L|Ls], Result) :-
	append(L, Tail, Result),
	append(Ls, Tail).

numlist(X, X, [X]) :- !.
numlist(X, Y, [X|L]) :-
	succ(X, Xn),
	numlist(Xn, Y, L).
