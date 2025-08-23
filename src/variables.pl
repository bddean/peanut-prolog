:- module(variables, [numbervars_allocation/2]).


 numbervars_allocation(T, allocate_vars(Varnames)) :-
   numbervars(Clauses, 0, NumVars),
        numlist(0, NumVars, VarNums),
        maplist(var_name_, VarNums, VarNames).

var_name_('$VAR'(N), $(S)) :-
  format(string(S), "_V~w", [N]).