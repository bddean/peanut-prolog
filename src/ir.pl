:- module(ir, [
	ir_type/3,
	walk_ir/3
]).

%! ir_node(Name, Spec, Description).
ir_type(','/2          , "Statement sequence"   , (goal:ir, goals:ir)).
ir_type('*->'/2        , "for...of loop"        , (generator:ir *-> body:ir)).
ir_type('->'/2         , "if statement"         , (condition_expr:ir -> body:ir)).
ir_type(':='/2         , "Init a const"         , name:ir := value:ir).
ir_type('yield_all'/1  , "yield multiple values", yield_all(generator:ir)).
ir_type(yield/0        , "yield once"           , yield).
ir_type(funcall/2      , "Call a function"      , funcall(name:atom, args: maplist(ir))).
ir_type(':'/2          , "Labelled block"       , (ident:ir) : (block:ir)).
ir_type(break/1        , "Exit labelled block"  , break(label:ir)).
ir_type(allocate_vars/1, "Make local logic vars", allocate_vars(names:maplist(ir))).
ir_type(
	make_term/2,
	"Manually construct a term. Usually you'd want \\/1 instead.

Note that args is `ir` not `maplist(ir)` -- it's not a prolog list but
an expression that evaluates to a host-language list.",
	make_term(tag:ir, args:ir)
).
ir_type(arguments/0, "Arguments list for current function", arguments).

ir_type('$'/1  , "atom identifier", $(name:atom)).
ir_type('$'/2  , "predicate ident", $(name:atom, arity:number)).
ir_type('$.'/1 , "Compiler-internal atom identifier" , $.(_:ir)).

ir_type(
	as/2,
	"Alias in import list",
	as(foreign_name:ir, local_name:ir)
).

ir_type(defun/3, "Define a named function in module scope", defun(
  type: fn_type_,
	name: ir,
	body: ir
)).

%%%%%% IR instructions related to modules %%%%%%
ir_type(
	declare_module/1,
	"",
	declare_module(name:atom)
).

ir_type(
	import/2,
	"Import statement compiled from a `:- use_module(...) directive.

One minor gotcha: The path argument isn't transformed to
allow backends to allow each backend to integrate better
with its host system.
",
	import(path:term, predicate_specs:maplist(ir))
).

ir_type(import_all/1, "import *", import_all(path:term)).

ir_type(
	export/1,
	"See import/2",
	export(predicate_specs:maplist(ir))
).

% TODO We're missing a few types here -- double check backend code.
% TODO Integrate type checks into our other compiler tests.

fn_type_(generator).
fn_type_(predicate).

fn_name_(Name/Arity) :- atom(Name), number(Arity).

% walk_ir(+Tform)//
% -----------------
% Generic n-ary tree traversal for the in-memory IR used by the front-end.
% The traversal is *bottom-up* and *single-pass*: first the current node is
% offered to the transformation, afterwards the children are visited.

:- meta_predicate walk_ir(2, ?, ?).
% TODO: More efficient to emit "closure" as we go bottom
% up. Instead of pre-calling
walk_ir(G) --> walk_kids(G), tform_node(G).
tform_node(G, E0, E) :-
    (   call(G, E0, E)
    *-> true
    ;   E = E0
    ).

walk_kids(G, E0, E) :-
    (   walk_kids_(G, E0, E)
    *-> true
    ;   E = E0
    ).

walk_kids_(G, IR0, IR) :-
	functor(IR0, Tag, Arity),
	ir_type(Tag/Arity, _, Type),
	IR0 =.. [_|Args0],
	Type =.. [_|ArgTypes],
	maplist(walk_kids__descend_(G), ArgTypes, Args0, Args),
	IR =.. [Tag|Args].

walk_kids__descend_(G, Type, Kid0, Kid) :-
	Type = _:ir *-> call(walk_ir(G), Kid0, Kid)
	; Type = _:maplist(ir) *-> maplist(walk_ir(G), Kid0, Kid)
	; Kid = Kid0.
