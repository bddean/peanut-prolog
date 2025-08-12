write_each([]).
write_each([X|Xs]) :- writeln(X), write_each(Xs).

main :-
	append([[1,2,3], [], [10, 100, 1000]], L),
	write_each(L).

:- main.
