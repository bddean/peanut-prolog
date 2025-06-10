:- ensure_loaded("pl-runtime").

% Basic Prolog predicates
A = A.

%%% List operations %%%
member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

append([], L, L).
append([H|T], L, [H|Result]) :- append(T, L, Result).

% TODO meta_predicate decl...
apply(G0, Xs1) :-
	add_args_(Xs1, G0, G),
	call(G).

add_args_(Xs, M:G0, M:G) :-	!, add_args_(Xs, G0, G).
add_args_(Xs1, G0, G) :-
	G0 =.. [Tag|Xs0],
	append(Xs0, Xs1, Xs),
	G =.. [Tag|Xs].

call(G, A, B, C, D, E, F, G, H, I) :- apply(G, [A, B, C, D, E, F, G, H, I]).
call(G, A, B, C, D, E, F, G, H)    :- apply(G, [A, B, C, D, E, F, G, H]).
call(G, A, B, C, D, E, F, G)       :- apply(G, [A, B, C, D, E, F, G]).
call(G, A, B, C, D, E, F)          :- apply(G, [A, B, C, D, E, F]).
call(G, A, B, C, D, E)             :- apply(G, [A, B, C, D, E]).
call(G, A, B, C, D)                :- apply(G, [A, B, C, D]).
call(G, A, B, C)                   :- apply(G, [A, B, C]).
call(G, A, B)                      :- apply(G, [A, B]).
call(G, A)                         :- apply(G, [A]).
