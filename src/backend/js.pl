:- module(js, [js/3, js/2]).
:- use_module(js_identifier, [js_escape_ident/2]).
:- use_module(library(gensym), [gensym/2]).
:- use_module('../comp_context').

% Public interface
js(T, S) :- js(T, S, []).

% Utils
fun_name(Name/Arity) --> js_atom(Name), "_", js(\Arity).

fun_ident(Name/Arity) -->
	{
		fun_name(Name/Arity, FName, []),
		atom_codes(A, FName),
		js_escape_ident(A, AIdent),
		atom_codes(AIdent, Ident),

		% Hack to support not-explicitly-qualified predicates
		( predicate_is_explicitly_defined(Name, Arity)
		-> Prefix = ""
		; Prefix = "$_STARMODS."
		)
	},
	Prefix, Ident.

%%
%% JavaScript code generation
%%
js(file_start) -->
	"const ", js($.("STARMODS")), " = ", "{};\n",
	"import { Var, Term, registerModule } from 'pl-runtime';\n".

js($(Name, Arity)) --> fun_ident(Name/Arity).
js(make_term(Name, Args)) --> "new Term(", Name, ",", Args, ")".
js(arguments) --> "args".

% Generator function
js(defun(generator, Ident, Body)) -->
	"function* ", Ident, "(...args) { \n",
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
js(\[]) --> "Symbol.for(\"[|]\")", !.  % SWI specific thing
js(\N) --> { number(N), number_codes(N, Codes) }, Codes.
js(\Term) -->	 { string(Term) }, "\"", js_escape(Term), "\"".
js(\Term) -->
	{ atom(Term), atom_string(Term, TermStr) },
	% TODO: Consider changing atoms to symbols in JavaScript
	"Symbol.for(\"", js_escape(TermStr), "\")".

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

js(declare_module(Name)) -->
	"registerModule(", js(\Name), ", s => eval(s));\n".

js(import(Path, Specs)) -->
	{ path_pl_to_js(Path, JsMod) },
	"import {", js_args(Specs), "} from ", js(\JsMod), ";\n".

js(export([])) -->	!, "".
js(export(Specs)) -->	"export {", js_args(Specs), "};\n".

% TODO: The globalThis trick we use is only approximately correct.
% Use static analysis across files instead.
js(import_all(Path)) -->
	{ path_pl_to_js(Path, JsMod) },
	{ gensym("mod", Tmp) },
	"import * as ", js($.(Tmp)), " from ", js(\JsMod), ";\n",
	"Object.assign(", js($.("STARMODS")), ", ", js($.(Tmp)),");\n".

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

