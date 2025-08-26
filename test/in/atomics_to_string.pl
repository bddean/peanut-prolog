main :-
	L = [1, 'singular', "sensation"],
	atomics_to_string(L, S0),
	writeln(S0),
	atomics_to_string(L, " ", S1),
	writeln(S1).

:- main.
