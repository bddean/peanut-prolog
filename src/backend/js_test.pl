:- use_module(js, [js/3, js/2]).

:- begin_tests(js_backend).
test(simple_unify, [nondet]) :-
	phrase(js(funcall('=', ["1", "1"])), Codes),
	string_codes(Result, Codes),
	assertion("$003D_2(1, 1)" == Result).

test(simple_funcall, [nondet]) :-
	phrase(js(funcall(test, [])), Codes),
	string_codes(Result, Codes),
	assertion("test_0()" == Result).

test(funcall_with_args) :-
	phrase(js(funcall(test, ["\"X\"", "123"])), Codes), !,
	string_codes(Result, Codes),
	assertion("test_2(\"X\", 123)" == Result).

test(term_literal) :-
	phrase(js(\foo(a)), Codes), !,
	string_codes(Result, Codes),
	sub_string(Result, _, _, _, "new Term"), !.

test(js_atom_dollar) :-
	phrase(js($test), Codes),
	string_codes(Result, Codes),
	Result = "test",
	!.

test(js_predicate_name, [nondet]) :-
	phrase(js($('=', 2)), Codes),
	string_codes(Result, Codes),
	assertion(Result == "$003D_2").

test(var_term) :-
	phrase(js(\('$VAR'(0))), Codes),
	!,
	string_codes(Result, Codes),
	assertion(Result == "$_0").

test(var_term_in_compound) :-
	Term = foo('$VAR'(0), '$VAR'(1)),
	phrase(js(\Term), Codes),
	string_codes(Result, Codes),
	sub_string(Result, _, _, _, "new Term"),
	!.

test(compound_term) :-
	Term = point(10, 20),
	phrase(js(\Term), Codes),
	!,
	string_codes(Result, Codes),
	format('Compound term result: ~w~n', [Result]),
	sub_string(Result, _, _, _, "new Term"), !.

:- end_tests(js_backend).
