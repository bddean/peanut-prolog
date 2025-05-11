:- module(simple_apply, []).

say(X, Y) :- writeln(X), writeln(Y).
stutter(Fill, X, Y) :-
	writeln(X), writeln(Fill),
	writeln(Y), writeln(Fill).

top :-
	apply(simple_apply:say, ["hello", "world"]),
	call(simple_apply:say, "hi", "you"),
	apply(simple_apply:stutter("uh"), ["bye", "planet"]),
	call(simple_apply:stutter(uh), "see", "ya").
