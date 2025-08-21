:- ensure_loaded(library(strings)).

test_string_length :-
	writeln('--- string_length/2 ---'),
	string_length("", L0),
	writeln(L0),
	string_length("hello", L1),
	writeln(L1),
	string_length("hello world", L2),
	writeln(L2),
	string_length("αβγ", L3),
	writeln(L3).

test_string_code :-
	writeln('--- string_code/3 ---'),
	string_code(1, "abc", C1),
	writeln(C1),
	string_code(2, "abc", C2),
	writeln(C2),
	string_code(3, "abc", C3),
	writeln(C3),
	string_code(1, "hi", H1),
	writeln(H1),
	string_code(2, "hi", H2),
	writeln(H2).

test_string_codes :-
	writeln('--- string_codes/2 ---'),
	string_codes("hello", Cs1),
	maplist(writeln, Cs1),
	string_codes("", _),
	writeln(empty),
	string_codes(S1, [65, 66, 67]),
	writeln(S1),
	string_codes(S2, [104, 101, 108, 108, 111]),
	writeln(S2).

test_string_chars :-
	writeln('--- string_chars/2 ---'),
	string_chars("abc", Chars1),
	maplist(writeln, Chars1),
	string_chars("", _),
	writeln(empty),
	string_chars(S1, [h, e, l, l, o]),
	writeln(S1),
	string_chars("test", Chars3),
	maplist(writeln, Chars3).

test_string_concat :-
	writeln('--- string_concat/3 ---'),
	string_concat("hello", "world", S1),
	writeln(S1),
	string_concat("", "test", S2),
	writeln(S2),
	string_concat("test", "", S3),
	writeln(S3),
	string_concat("a", "b", S4),
	writeln(S4).

test_string_case :-
	writeln('--- string_upper/2 and string_lower/2 ---'),
	string_upper("hello", U1),
	writeln(U1),
	string_upper("HeLLo WoRLd", U2),
	writeln(U2),
	string_lower("HELLO", L1),
	writeln(L1),
	string_lower("HeLLo WoRLd", L2),
	writeln(L2).

test_atom_string :-
	writeln('--- atom_string/2 ---'),
	atom_string(hello, S1),
	writeln(S1),
	atom_string(A1, "world"),
	writeln(A1),
	atom_string(test, S2),
	writeln(S2).

main :-
	test_string_length,
	test_string_code,
	test_string_codes,
	test_string_chars,
	test_string_concat,
	test_string_case,
	test_atom_string.

:- main.