:- use_module(js_identifier).

:- begin_tests(js_identifier_test).

test(all_examples) :-
        Pairs = [
            test-"test",
            "hello"-"hello",
            'hello-world'-"hello$002Dworld",
            '123abc'-"$003123abc",
            'café'-"café",
            'my$var'-"my$0024var",
            ''-"",
            'foo bar'-"foo$0020bar",
            '_private'-"_private",
            'x!y@z#'-"x$0021y$0040z$0023",
            'αβγ'-"αβγ"
        ],
        forall(member(P-E, Pairs),
               round_trip(P, E)).

round_trip(PlainSpec, EscSpec) :-
        atom_string(Plain, PlainSpec),   % allow atoms or strings
        atom_string(Esc  , EscSpec),
        js_escape_ident(Plain, Result),           % encode
				assertion(Result == Esc),
        js_escape_ident(Back, Esc),           % decode
        assertion(Back == Plain).

:- end_tests(js_identifier_test).
