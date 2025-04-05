:- module(test, [pg/1]).

(G ; _) :- call(G).
(_ ; G) :- call(G).

pg(X) :- g(X), p(X).

g(1).
g(2).
g(3).

p(2).
p(3).
p(4).

:-
	g(X),
	p(X),
	writeln(X),
	fail ; p(X), writeln(X), writeln(ok), fail.
