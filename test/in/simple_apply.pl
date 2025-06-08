say(X, Y) :- writeln(X), writeln(Y).
stutter(Fill, X, Y) :-
	writeln(X), writeln(Fill),
	writeln(Y), writeln(Fill).

top :-
	apply(user:say, ["hello", "world"]),
	call(user:say, "hi", "you"),
	apply(user:stutter("uh"), ["bye", "planet"]),
	call(user:stutter(uh), "see", "ya").
