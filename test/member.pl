member(X, [X]).
member(X, [_|L]) :- member(X, L).
