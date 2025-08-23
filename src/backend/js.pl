:- module(js, [js/3, js/2]).
:- use_module(js_identifier, [js_escape_ident/2]).
:- use_module(library(gensym), [gensym/2]).
:- use_module('../directives', [compile_current_module/1]).

% Public interface
js(T, S) :- js(T, S, []).


%%
%% JavaScript code generation
%%
js(file_start) -->
	"import { Var, makeTerm, registerModule, db_set, db_get } from 'pl-runtime';\n".

js(make_term(Name, Args)) --> "makeTerm(", Name, ",", Args, ")".
js(arguments) --> "args".

js(fn(generator, Body)) -->
	"function* (...args) { \n",
		Body,
	"\n}".

js(db_set(Module, Name, Arity, X)) -->
	{
		format(string(Key), "~w:~w/~w", [Module, Name, Arity])
	},
	"db_set(", js(\Key), ", ", X, ");\n".


% Function call
js(funcall(Module, Name, Args)) -->
	{
		length(Args, N),
		format(string(Key), "~w:~w/~w", [Module, Name, N])
	},
	"db_get(", js(\Key), ")(", js_args(Args), ")".

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
js(\'$PEANUT VAR'(N)) -->
	{ format(string(Name), "~d", [N]) },
	!,
	js($.(Name)).
js(\[]) --> "Symbol.for(\"[|]\")", !.  % SWI specific thing
js(\N) --> { number(N), number_codes(N, Codes) }, Codes.
js(\Term) --> { string(Term) }, "\"", js_escape(Term), "\"".
js(\Term) -->
	{ atom(Term), atom_string(Term, TermStr) },
	"Symbol.for(\"", js_escape(TermStr), "\")".

js(\Term) -->
	%% Handle any other compound terms
	{ compound(Term), \+ Term = '$PEANUT VAR'(_) },
	{ compound_name_arguments(Term, Functor, Args) },
	"makeTerm(", js(\Functor), ", [", js_term_args(Args), "])".

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

js(declare_module(Name)) -->
	"registerModule(", js(\Name), ", s => eval(s));\n".

js(import(Path)) -->
	{ path_pl_to_js(Path, JsMod) },
	"import ", js(\JsMod), ";\n".


%%%%%% Helper predicates %%%%%%%
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

path_pl_to_js(Fn, S) :-
	Fn =.. [Tag,Path], !,
	path_package(Tag, Root),
	path_string(Root/Path, S).
path_pl_to_js(P, S) :- path_string(P, S).

path_string(R/P, S) :- !,
	path_string(P, Ps),
	atomics_to_string([R, '/', Ps], S).
path_string(A, S) :- atomics_to_string([A], S).

path_package(library, "pl-library").
path_package(runtime, "pl-runtime").

:- begin_tests(js_paths). %%%%%%%%%%%%%%%%%%%%%%%%
	test(lib, []) :-                               %
		path_pl_to_js(library(lists/subpath), W),    %
		assertion(W == "pl-library/lists/subpath"). %
                                                 %
	test(relative) :-                              %
		path_pl_to_js("./a/b/c", W),                 %
		assertion(W == "./a/b/c").                   %
:- end_tests(js_paths). %%%%%%%%%%%%%%%%%%%%%%%%%%

