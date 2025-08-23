:- module(variables, [numbervars_allocation/2]).

numbervars_allocation(T, allocate_vars(Names)) :-
  % TODO: Guaruntee functor does not appear T.
	numbervars(T, 0, Count, [functor_name('$PEANUT VAR')]),
	numlist(0, Count, VarNums),
	maplist(var_name_, VarNums, Names).

var_name_(N, $.(S)) :- number_string(N, S).
