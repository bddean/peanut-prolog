:- ensure_loaded(library(math)).

length([], 0).
length([H|T], N) :- length(T, N0), succ(N0, N).

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

maplist(_, []).
maplist(G, [X|Xs]) :- call(G, X), maplist(G, Xs).

maplist(_, [], []).
maplist(G, [A|As], [B|Bs]) :- call(G, A, B), maplist(G, As, Bs).

maplist(_, [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs]) :- call(G, A, B, C), maplist(G, As, Bs, Cs).

maplist(_, [], [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs], [D|Ds]) :- call(G, A, B, C, D), maplist(G, As, Bs, Cs, Ds).

%! foldl(G, Xs, X0, X)
foldl(_, [], X, X).
foldl(G, [H|T], X0, X) :-
	call(G, H, X0, X1),
	foldl(G, T, X1, X).
