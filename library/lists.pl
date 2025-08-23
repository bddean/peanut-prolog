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

numbervars(T, N0, N) :-	numbervars(T, N0, N, []).

numbervars(T, N0, N, _Opts) :-
  term_variables(T, Vs),
	length(Vs, Len),
	N1 is N0 + Len - 1,
  numlist(N0, N1, Ns),
  maplist(numbervars__var_('$VAR'), Ns, Vs),
	N is N1 + 1.

numbervars__var_(FunctorName, N, Wrapped) :-
	Wrapped =.. [FunctorName, N].

numbervars__opts_(_, '$VAR') :- !. %% TODO see below...
numbervars__opts_(
	Opts,
	FunctorName
	%% AttVar -- not supported yet
	%% singletons -- not supported yet
) :-
	% TODO -- seems like a weird bug even with append and memberchk
	append(Opts, [functor_name('$VAR')], OptsWithDefaults),
  memberchk(functor_name(FunctorName), OptsWithDefaults).

maplist(_, []).
maplist(G, [X|Xs]) :- call(G, X), maplist(G, Xs).

maplist(_, [], []).
maplist(G, [A|As], [B|Bs]) :- call(G, A, B), maplist(G, As, Bs).

maplist(_, [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs]) :- call(G, A, B, C), maplist(G, As, Bs, Cs).

maplist(_, [], [], [], []).
maplist(G, [A|As], [B|Bs], [C|Cs], [D|Ds]) :- call(G, A, B, C, D), maplist(G, As, Bs, Cs, Ds).
