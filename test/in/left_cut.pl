message :-
	( writeln("expect to see this"),
	  !
	; writeln("expect NOT to see this")
  ),
	( writeln("ALSO expect to see this")
	; writeln("AND ALSO to see this")
	).

top :- message, fail ; true.
