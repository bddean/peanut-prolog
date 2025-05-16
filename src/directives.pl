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

directive_ir(use_module(Path), import_all(Path)).
directive_ir(
	use_module(Path, Plmods),
	import(Path, Hostmods)
) :- maplist(spec_ir_, Plmods, Hostmods).

directive_ir(
	module(Name, PlMods),
	(
		declare_module(AName),
		export(HostMods)
	)
) :-
	atomic_list_concat([Name], AName),
	maplist(spec_ir_, PlMods, HostMods).

directive_ir(
	module(Name),
	declare_module(AName)
) :- atomic_list_concat([Name], AName).

%% TODO this can be done with goal_expansion when we have
%% host fn literals.
register_metapredicate_(Spec) :-
	functor(Spec, Tag, Arity),
	assertz(metapred_spec(Tag/Arity, Spec)).

spec_ir_(
	Pred/Arity as Name,
	Fun as $(Name, Arity)
) :- !,	spec_ir_(Pred/Arity, Fun).

spec_ir_(A/N, $(A, N)).
