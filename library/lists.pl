hmember(X, [X|_]).
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

numbervars(T, N0, N) :-
  term_variables(T, Vs),
  numlist(N0, N, Ns),
  maplist(numbervars__var_, Ns, Vs).

numbervars__var_(N, '$VAR'(N)).

maplist(_, []).
maplist(G, [X|Xs]) :- call(G, X), maplist(G, Xs).

maplist(_, [], []).
maplist(G, [A|As], [B|Bs]) :- call(G, A, B), maplist(G, As, Bs).

maplist(_, [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs]) :- call(G, A, B, C), maplist(G, As, Bs, Cs).

maplist(_, [], [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs], [D|Ds]) :- call(G, A, B, C, D), maplist(G, As, Bs, Cs, Ds).
