test_get_length :-
	L = [1,2],
	length(L, N),
	writeln(N),
	N = 2,
	writeln(ok).

test_get_list :-
	length(L, 2),
	maplist(=(a), L),
	maplist(writeln, L),
	writeln(ok).

test_generate :-
	length(L, N),
	maplist(=(N), L),
	maplist(writeln, L),
	N = 2,
	writeln(done).


main :-
	test_get_length,
	test_get_list,
	test_generate.

:- main.
