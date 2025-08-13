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

peel_col_(
  [[First|Row] | FullRows],
  [First|Col],
  [Row|PeeledRows]
) :-
	peel_col_(FullRows, Col, PeeledRows).

transpose([], []).
transpose(Rows0, [C|Cs]) :-
	peel_col_(Rows0, C, Rows1),
	transpose(Rows1, Cs).


maplist(G, []).
maplist(G, [X|Xs]) :- call(G, X), maplist(G, Xs).

maplist_(G, Ls) :-
	transpose(Ls, Args),
	maplist(apply(G), Args).

maplist(G, A, B)                   :- maplist_(G, [A, B]).
maplist(G, A, B, C)                :- maplist_(G, [A, B, C]).
maplist(G, A, B, C, D)             :- maplist_(G, [A, B, C, D]).
maplist(G, A, B, C, D, E)          :- maplist_(G, [A, B, C, D, E]).
maplist(G, A, B, C, D, E, F)       :- maplist_(G, [A, B, C, D, E, F]).
maplist(G, A, B, C, D, E, F, G)    :- maplist_(G, [A, B, C, D, E, F, G]).
maplist(G, A, B, C, D, E, F, G, H) :- maplist_(G, [A, B, C, D, E, F, G, H]).
