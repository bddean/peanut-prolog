
greeting --> [hello].
target --> [world].
grammar --> greeting, target.

main :-
	grammar(W, []),
	maplist(writeln, W).

:- main.
