test_simple :-
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

test_custom_functor :-
	G = foo(X, _),
	T = bar(X, asdf, G),
	numbervars(T, 100, N, [functor_name(eden)]),
	writeln(T),
  writeln(N).

main :-
	test_simple,
	test_custom_functor.

:- main.
