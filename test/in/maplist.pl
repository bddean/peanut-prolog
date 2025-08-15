plusnlog(A, B, C) :-
	writeln('??'),
	plus(A, B, C),
	writeln('++'),
	writeln(A),
	writeln(B),
	writeln(C).

main :-
	maplist(writeln, [a,b,c]),
	maplist(
		plusnlog,
		[1, 2, A],
		[10, B, 30],
		[C, 200, 300]
	),
	maplist(writeln, [A, B, C]).

:- main.
