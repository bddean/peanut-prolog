:- module(directives, [
	directive_ir/2,
	metapred_spec/2,
	compile_current_module/1
]).
:- use_module("./helpers").

:- dynamic
	metapred_spec/2,
	compile_current_module/1.

% By default we assume all unqualified calls live in module `user`.

compile_current_module(user).

directive_ir((meta_predicate SpecArgs), []) :-
	args_list(SpecArgs, Specs),
	maplist(register_metapredicate_, Specs).

directive_ir(use_module(Path), import(Path)).
directive_ir(use_module(Path, _), import(Path)).

directive_ir(
	module(Name, _),
	declare_module(AName)
) :-
	atomic_list_concat([Name], AName),
	retractall(compile_current_module(_)),
	assertz(compile_current_module(Name)).

directive_ir(
	module(Name),
	declare_module(AName)
) :- 
	atomic_list_concat([Name], AName),
	retractall(compile_current_module(_)),
	assertz(compile_current_module(Name)).

%% TODO this can be done with goal_expansion when we have
%% host fn literals.
register_metapredicate_(Spec) :-
	functor(Spec, Tag, Arity),
	assertz(metapred_spec(Tag/Arity, Spec)).

