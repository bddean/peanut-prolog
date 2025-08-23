main :-
	T = f(A, g(B, 3)),
	term_variables(T, Vars),

	maplist(var, Vars),
	numlist(1, 2, Nums),
	Vars = Nums,
	writeln(a(A)),
	writeln(b(B)).

:- main.
