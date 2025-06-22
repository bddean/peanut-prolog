write_each([]).
write_each([X|Xs]) :- writeln(X), write_each(Xs).

main :-
	numlist(10, 20, W),
	write_each(W).

:- main.
