:- module(js, [js/3, js/2, ir_to_js/2]).
:- use_module(js_identifier, [js_escape_ident/2]).

% Public interface
js(T, S) :- js(T, S, []).

% Compile IR to JavaScript
% TODO not like this...
ir_to_js(IR, JSCode) :-
	js(IR, Codes),
	string_codes(JSCode, Codes).

%%
%% JavaScript code generation
%%

% Tracing clause for debugging
% Generator function
js(defun(generator, Name/Arity, Body)) -->
	"function* ", js_atom(Name), "_", js(\Arity), "(...args) { \n",
	"const CALLED_TERM = ", called_term_expr_(Name/Arity), ";\n",
	Body,
	"\n}".

% Function call
js(funcall(Name, Args)) -->
	{ atom_codes(Name, NameCodes) },
	NameCodes, "(", js_args(Args), ")".

js(Name := Value) --> "const ", Name, "=", Value, ";\n".

js(allocate_vars(VarNameList)) -->
	"const [", js_args(VarNameList), "] = Var.allocate();\n".

% Control structures
js(!) --> "break;".
js(nothing) --> "".
js([]) --> "".
js(yield) --> "yield;\n".
js(yield_all(G)) --> "yield* ", G, ";\n".
js((A, B)) --> A, ";\n", B.

js((Cond -> Block)) -->
	"if (", Cond, ") {\n",
	Block,
	"\n}".

js((Gen *-> Block)) -->
	"for (const _ of ", Gen, ") {\n",
	Block,
	"\n}".

% Term literals
js(\'$VAR'(N)) -->
	{ format(string(Name), "_~d", [N]) },
	!,
	js($Name).
js(\[]) --> "\"[|]\"", !.  % SWI specific thing
js(\N) --> { number(N), number_codes(N, Codes) }, Codes.
js(\Term) -->	 { string(Term) }, "\"", js_escape(Term), "\"".
js(\Term) -->
	{ atom(Term), atom_string(Term, TermStr) },
	% TODO: Consider changing atoms to symbols in JavaScript
	js(\TermStr).
js(\Term) -->
	%% Handle any other compound terms
	{ compound(Term), \+ Term = '$VAR'(_) },
	{ functor(Term, Functor, _) },
	{ Term =.. [Functor|Args] },
	"new Term(", js(\Functor), ", [", js_term_args(Args), "])".

% Variables and literals
% $ operator is for JS identifiers (variable names)
js($X) -->
	{ js_escape_ident(X, JSIdent) },
	{ string_codes(JSIdent, Codes) },
	Codes.

% Helper predicates
js_args([]) --> "".
js_args([Arg]) --> Arg.
js_args([Arg1, Arg2 | Args]) --> Arg1, ", ", js_args([Arg2 | Args]).

% Helper for term arguments
js_term_args([]) --> "".
js_term_args([Arg]) --> js(\Arg).
js_term_args([Arg1, Arg2 | Args]) -->
	js(\Arg1), ", ", js_term_args([Arg2 | Args]).

js_atom(A) --> { atom(A), atom_codes(A, Codes) }, Codes.
js_atom(S) --> { string(S), string_codes(S, Codes) }, Codes.

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

called_term_expr_(Name/0) --> js(\Name).
called_term_expr_(Name/Arity) --> { Arity > 0 },
	"new Term(", js(\Name), ", args)".


% TODO nearly all these tests are bad
% b/c they weren't rewritten when I changed the
% interface
%
% probably should be replaced by integration tests anyhow.
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
	phrase(js(defun(generator, foo/0, funcall("test", []))), Codes),
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
