main :-
	( true -> writeln(good) ; writeln(bad) ),
	( false -> writeln(no) ; writeln(yes) ),
	( 1=1 -> writeln(done) ).

:- main.
