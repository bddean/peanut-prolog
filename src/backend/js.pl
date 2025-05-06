:- module(js, [js/3, js/2]).
:- use_module(js_identifier, [js_escape_ident/2]).

% Public interface
js(T, S) :- js(T, S, []).

fun_name(Name/Arity) --> js_atom(Name), "_", js(\Arity).
fun_ident(Name/Arity) -->
	{
		fun_name(Name/Arity, FName, []),
		atom_codes(A, FName),
		js_escape_ident(A, AIdent),
		atom_codes(AIdent, Ident)
	},
	Ident.
%%
%% JavaScript code generation
%%

% Generator function
js(defun(generator, Name/Arity, Body)) -->
	"function* ", fun_ident(Name/Arity), "(...args) { \n",
	"const ", js($.("CALLED_TERM")), " = ", called_term_expr_(Name/Arity), ";\n",
	Body,
	"\n}".

% Function call
js(funcall(Name, Args)) -->
	{ length(Args, N) },
	fun_ident(Name/N), "(", js_args(Args), ")".

js(Name := Value) --> "const ", Name, "=", Value, ";\n".

js(allocate_vars(VarNameList)) -->
	"const [", js_args(VarNameList), "] = Var.allocate();\n".

js(nothing) --> "".

% Control structures
js(Label:Block) --> Label, ":", "{\n", Block, "\n}".
js(break(Label)) --> "break ", Label, ";\n".
js(return) --> "return;\n".
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
	{ format(string(Name), "~d", [N]) },
	!,
	js($.(Name)).
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
% Compiler-generated identifiers -- separate namespace from
% user-visible idents like for predicate names.
js($.(X)) -->
	{ format(string(Prefixed), "_~s", [X]) },
	"$", js($Prefixed).

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
