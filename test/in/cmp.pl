main :-
	One = 1,
	Ten = 10,
	One < Ten,
	One < 99,
	One =< 99,
	Ten > One,
	Ten >= 1,
	nonvar(One),
	nonvar(Ten),
	writeln("done").

:- main.
