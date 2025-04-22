:- module(js, [js/3, js/2, ir_to_js/2]).

% Public interface
js(T, S) :- js(T, S, []).

js(V) :- var(V), throw(is_var).
js(\V) :- var(V), throw(is_var).

% Compile IR to JavaScript
% nosubmit not like htis
ir_to_js(IR, JSCode) :-
	writeln('Starting JavaScript generation...'),
	writeln('IR:'),
	portray_clause(IR),
	(catch(
	    (phrase(js(IR), Codes) ->
	        string_codes(JSCode, Codes),
	        writeln('JavaScript generation succeeded')
	    ;
	        writeln('DCG failed'),
	        fail
	    ),
	    JSError,
	    (writeln('Error during JavaScript generation:'), writeln(JSError), fail)
	) ->
	    true
	;
	    writeln('JavaScript generation failed'),
	    fail
	).

%%
%% JavaScript code generation
%%

% Tracing clause for debugging
% Generator function
js(defun(generator, Name/Arity, Arg, Body)) -->
	"function* $", js_atom(Name), "_", js_num(Arity), "(", js_atom(Arg), ") {\n",
	js(Body),
	"\n}".

% Function call
js(funcall(Name, Args)) -->
	{ format('Processing funcall: ~w~n', [Name]) },
	{ format('Args: ~w~n', [Args]) },
	{ atom_codes(Name, NameCodes) },
	NameCodes, "(", js_args(Args), ")".

% Control structures
js(!) --> "break;".
js(nothing) --> "".
js([]) --> "".

js((Cond -> Block)) -->
	"if (", js(Cond), ") {\n",
	js(Block),
	"\n}".

js((Gen *-> Block)) -->
	"for (const _ of ", js(Gen), ") {\n",
	js(Block),
	"\n}".

js((S1 ; S2)) -->
	js(S1), ";\n",
	js(S2).

% Term literals
js(\Term) -->
	{ Term = '$VAR'(N), integer(N) }, !,
	"new Var() /* ", js_num(N), " */".
js(\Term) -->
	{ number(Term) },
	js_num(Term).
js(\Term) -->
	{ string(Term) },
	"\"", js_escape(Term), "\"".
js(\Term) -->
	{ atom(Term), atom_string(Term, TermStr) },
	"\"", js_escape(TermStr), "\"".  % TODO: Consider changing atoms to symbols in JavaScript
js(\Term) -->
	%% Handle any other compound terms
	{ compound(Term), \+ Term = '$VAR'(_) },
	{ functor(Term, Functor, _) },
	{ Term =.. [Functor|Args] },
	"new Term(\"", js_escape(Functor), "\", [", js_term_args(Args), "])".

% Variables and literals
% $ operator is for JS identifiers (variable names)
js($X) --> js_atom(X).

% Helper predicates
js_args([]) --> "".
js_args([Arg]) --> js(Arg).
js_args([Arg1, Arg2 | Args]) -->
	js(Arg1), ", ", js_args([Arg2 | Args]).

% Helper for term arguments
js_term_args([]) --> "".
js_term_args([Arg]) --> js(\Arg).
js_term_args([Arg1, Arg2 | Args]) -->
	js(\Arg1), ", ", js_term_args([Arg2 | Args]).

js_atom(A) --> { atom(A), atom_codes(A, Codes) }, Codes.
js_atom(S) --> { string(S), string_codes(S, Codes) }, Codes.
js_num(N) --> { number(N), number_codes(N, Codes) }, Codes.
js_string(S) --> js_escape(S).


% Escape special characters in strings
js_escape(Str) -->
	{ string_codes(Str, Codes) },
	js_escape_codes(Codes).

js_escape_codes([]) --> [].
js_escape_codes([C|Cs]) -->
	js_escape_code(C),
	js_escape_codes(Cs).

% Escape special characters
js_escape_code(0'\\) --> "\\\\".  % Backslash
js_escape_code(0'') --> "\\'".    % Single quote
js_escape_code(0'") --> "\\\"".   % Double quote
js_escape_code(0'\n) --> "\\n".   % Newline
js_escape_code(0'\r) --> "\\r".   % Carriage return
js_escape_code(0'\t) --> "\\t".   % Tab
js_escape_code(C) --> [C].        % Regular character

%% Unit tests
% Direct test for debugging
debug_var_term :-
    V = '$VAR'(0),
    format('V = ~w~n', [V]),
    (compound(V) -> format('V is compound~n', []) ; format('V is NOT compound~n', [])),
    (V = '$VAR'(N), integer(N) -> format('V matches $VAR pattern with N = ~w~n', [N]) ; format('V does NOT match $VAR pattern~n', [])),
    phrase(js(\V), Codes),
    string_codes(Result, Codes),
    format('Result: ~w~n', [Result]).

:- begin_tests(js_backend).

test(simple_funcall) :-
	phrase(js(funcall(test, [])), Codes),
	string_codes(Result, Codes),
	Result = "test()".

%% nosumbit replace / rename...
test(funcall_with_primitive_args) :-
	phrase(js(funcall("test", [\"X", \123])), Codes), !,
	string_codes(Result, Codes),
		writeln(funcall_with_string_arg:result:Result),
	Result = "test(\"X\", 123)".

test(funcall_with_multiple_string_args) :-
	phrase(js(funcall("test", [$"X", $"Y"])), Codes), !,
	string_codes(Result, Codes),
	Result = "test(X, Y)".

test(term_literal) :-
	phrase(js(\foo(a)), Codes), !,
	string_codes(Result, Codes),
	sub_string(Result, _, _, _, "new Term"), !.

test(simple_generator) :-
	phrase(js(defun(generator, foo/0, "X", funcall("test", []))), Codes),
	!,
	string_codes(Result, Codes),
	sub_string(Result, 0, _, _, "function* $"),
		!.

% Direct tests for DCG helper rules
test(js_atom_direct) :-
	phrase(js_atom(test), Codes), !,
	string_codes(Result, Codes),
	Result = "test".

test(js_atom_with_var) :-
	phrase(js_atom("X"), Codes),
	string_codes(Result, Codes),
	Result = "X".

test(js_atom_dollar) :-
	phrase(js($test), Codes),
	string_codes(Result, Codes),
	Result = "test",
	!.

test(js_num_direct) :-
	phrase(js_num(42), Codes),
	string_codes(Result, Codes),
	Result = "42".

test(simple_unify_with_var) :-
	format('Testing simple unify with var~n', []),
	V = '$VAR'(0),
	phrase(js(funcall("unify", [\"X", \V])), Codes), !,
	string_codes(Result, Codes),
	Result = "unify(\"X\", new Var() /* 0 */)".

test(var_term) :-
	phrase(js(\('$VAR'(0))), Codes),
	!,
	string_codes(Result, Codes),
	sub_string(Result, _, _, _, "new Var()"), !.

test(var_term_in_compound) :-
	Term = foo('$VAR'(0), '$VAR'(1)),
	phrase(js(\Term), Codes),
	!,
	string_codes(Result, Codes),
	format('Term with vars result: ~w~n', [Result]),
	sub_string(Result, _, _, _, "new Term"), !.

test(compound_term) :-
	Term = point(10, 20),
	phrase(js(\Term), Codes),
	!,
	string_codes(Result, Codes),
	format('Compound term result: ~w~n', [Result]),
	sub_string(Result, _, _, _, "new Term"), !.

:- end_tests(js_backend).
