:- module(helpers_test, []).

:- use_module(helpers).

:- begin_tests(helpers).
test(from_list, [nondet]) :-
	args_list(
		W,
		[(a,b),c]
	),
	assertion(W == ((a, b), c)).
:- end_tests(helpers).
