:- module(comp_context, [
	ingest_term/1,
	predicate_is_explicitly_defined/2
]).

:- dynamic file_term/1.

ingest_term(T) :- assertz(file_term(T)).

predicate_is_explicitly_defined(SName, Arity) :-
	atomic_list_concat([SName], Name),
	length(Args, Arity),
	Template =.. [Name|Args],
	once((
		file_term(T),
		memberchk(T, [
			Template,
			(Template :- _),
			(:- use_module(_, Specs))
		]),
		memberchk(Name/Arity, Specs)
	)).
