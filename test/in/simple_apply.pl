say(X, Y) :- writeln(X), writeln(Y).
stutter(Fill, X, Y) :-
	writeln(X), writeln(Fill),
	writeln(Y), writeln(Fill).

main :-
	apply(say, ["hello", "world"]),
	call(say, "hi", "you"),
	apply(stutter("uh"), ["bye", "planet"]),
	call(stutter(uh), "see", "ya").
