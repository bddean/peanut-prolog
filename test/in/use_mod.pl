:- module(use_mod, []).
:- use_module(library(pairs)).

top :-
	In = [a-b, c-d],
	pairs_keys(In, [W|_]),
	writeln(W).
