:- module(goal_ir, [
	goal_ir/2,
	goal_ir/4
]).
:- use_module(directives).

% Convert Prolog statements to IR
% goal_ir(+Statement, -IR)
goal_ir(G, IR) :- goal_ir(function, yield, G, IR).

goal_ir(Scope, Cont, (A0, B0), A) :-
	!,
	goal_ir(Scope, Cont, B0, B),
	goal_ir(Scope, B, A0, A).

goal_ir(Scope, Cont, (A0 ; B0), (A , B)) :-
	!,
	maplist(
		goal_ir(Scope, Cont),
		[A0, B0],
		[A, B]
	).

goal_ir(function, Cont, !, (Cont, return)) :- !.
goal_ir(block(Lbl), Cont, !, (Cont, break($Lbl))) :- !.

% TODO convert to -> in second pass
goal_ir(_, Cont, Term, (Call *-> Cont)) :-
	term_call_ir(Term, Call).

% Convert a Prolog term to a function call
% term_call_ir(+Term, -Call)
term_call_ir(Term, funcall(Module, Name, Args)) :-
	Term =.. [Name|Args0],
	wrap_args(Args0, Args),
	compile_current_module(Module).

% Wrap arguments in the IR format
% wrap_args(+PrologArgs, -WrappedArgs)
wrap_args(Ts, As) :- maplist(wrap_args__escape_, Ts, As).
wrap_args__escape_(X, \X).
