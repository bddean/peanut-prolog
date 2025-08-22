test_univ :-
	writeln('--- =../2 tests ---'),
	% Test 1: decompose a compound term
	foo(a, b, c) =.. L1,
	maplist(writeln, L1),

	% Test 2: construct a compound term
	T2 =.. [bar, 1, 2],
	writeln(T2),

	% Test 3: atom to list
	hello =.. L3,
	maplist(writeln, L3),

	% Test 4: construct array from list with strings
	Arr =.. ['#', "hello", "world"],
	writeln(Arr),

	% Test 5: array construction with numbers
	Arr2 =.. ['#', 1, 2, 3],
	writeln(Arr2),

	% Test 6: the potentially failing case - univ with unbound first arg
	atom_string(h, H),
	atom_string(i, I),
	Subs = [H, I],
	maplist(writeln, Subs),
	X =.. ['#'|Subs],
	writeln(X),

	% Test 7: decompose the array we just made
	'#'("a", "b", "c") =.. L7,
	maplist(writeln, L7),

	% Test 8: decompose and reconstruct
	'#'(1, 2, 3) =.. DecompList,
	maplist(writeln, DecompList),
	Reconstructed =.. DecompList,
	writeln(Reconstructed),

	% Test 9: with mixed types
	'#'(atom, "string", 42) =.. L9,
	maplist(writeln, L9),

	% Test 10: double decomposition
	foo('#'(1, 2), bar) =.. OuterList,
	maplist(writeln, OuterList).

main :- test_univ.

:- main.
