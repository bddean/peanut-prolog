message :-
	( writeln("expect to see this"),
	  !
	; writeln("expect NOT to see this")
  )
	;	writeln("should also not see this").

top :- message, fail ; true.
