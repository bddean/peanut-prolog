%% Tests a bug that was in maplist.
main :-
	L0 = [a,b,c],
	maplist(=, L0, L),
	L = L0,
	writeln(L),
	writeln(L0).

:- main.

