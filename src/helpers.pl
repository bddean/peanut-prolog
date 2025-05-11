:- module(helpers, [
	args_list/2,
	args_list/3
]).

args_list(Args, Ls) :- args_list(nothing, Args, Ls).

args_list(Empty, Empty, []).
args_list(_, X, [X]).
args_list(Empty, (X, As), [X|Xs]) :-
	Xs=[_|_],
	arglist(Empty, Xs, As).
