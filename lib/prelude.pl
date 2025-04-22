% Basic Prolog predicates
A = A.

% List operations
member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

append([], L, L).
append([H|T], L, [H|Result]) :- append(T, L, Result).
