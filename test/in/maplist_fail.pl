g(_, _).

test_unequal_lengths :-
	L1 = [a],
	L2 = [x, y, z],
	maplist(g, L1, L2), !,
	writeln(fail).

test_unequal_lengths :-
	writeln(ok).

:- test_unequal_lengths.
