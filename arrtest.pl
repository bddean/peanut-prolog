main :-
	Cs = [a,b,c],
	maplist(=, Cs, In),
	array_list(A, In),
	writeln(to_array),
	writeln(A).


:- main.
