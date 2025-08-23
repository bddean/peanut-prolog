main :-
	% Simple test with a term containing variables
	T = foo(X, _Y, 100, X),
	numbervars(T, 0, N),
	T =.. [foo|Args],
	maplist(nonvar, Args),
	Args = ['$VAR'(Nx), '$VAR'(Ny)|_],
	writeln('Var nums:'),
	writeln(Nx),
	writeln(Ny),
	writeln('Next var number: '),
	writeln(N).

:- main.
