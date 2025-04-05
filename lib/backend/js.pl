:- module(js, [js/3, js/2]).

js(T, S) :- js(T, S, []).

%% Serialize any of the following types:
%% * defun(bool, ...)
%% * defun(generator, ...)
%% * funcall/2
%% * $/1 -- for identifiers
%% * if
%% * for
%% * !
%% * \ for actual pl value

js(!) --> "break;". %% TODO need loop labels probably...
js([]) --> "". %% TODO implement "abstract class" style?
js(defun(
	bool, % return type
  Name, % in: string
	Args, % in: a list of identifier names
	Body % in: block or expression -- already serialized.
)) -->
	"const ", Name, " = (",
		argnames(Args),
	") => {", Body, "}".

js(defun(
  generator,
	Name,
	Args,
	Body
)) --> "function*", Name, "(",
	argnames(Args), ") {",	Body, "}".

js(funcall(
	Name,
	Args
)) -->
	Name, "(", join_commas(Args), ")".

js($X) --> {term_string(X, S) }, S. %% TODO escaping and stuff.
js(ConditionJs -> BlockJs) --> "if(", ConditionJs, ")",  BlockJs.
js(IterableJs *-> BlockJs) -->
	"for (const _ of", IterableJs, ")",
		BlockJs.
js(B0 ; B1) --> "{", B0, ";", B1, "}".

%% Helper predicates:
join_commas([]) --> "".
join_commas([X]) --> X.
join_commas([X, Y | Xs]) --> X, ",", join_commas([Y|Xs]).

js_expr(T) --> T. %% nosubmit!

:- begin_tests(js).
:- use_module(js).
test(t1) :-
	phrase(
    js:js(funcall("testfn", ["1","2", "asdf"])),
		W
	),
	!,
	string_codes(S, W),
	S = "testfn(1,2,asdf)".


:- end_tests(js).
