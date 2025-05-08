:- module(directives, [
	directive_ir/2
]).

directive_ir(
	use_module(path, plmods),
	import(path, hostmods)
) :- maplist(spec_ir_, plmods, hostmods).

directive_ir(
	module(Name, PlMods),
	export($Name, HostMods)
) :- maplist(spec_ir_, PlMods, HostMods).

%directive_ir(module(Name, Exports), export()).

spec_ir_(
	Pred/Arity as Name,
	Fun as $(Name, Arity)
) :- !,	spec_ir_(Pred/Arity, Fun).

spec_ir_(A/N, $(A, N)).
