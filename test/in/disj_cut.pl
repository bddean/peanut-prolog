message :-
	writeln("expect to see this"),
	!,
	fail
	; writeln("expect NOT to see this").

main :- message ; true.
