:- module(directives, [
	directive_ir/2,
	compile_current_module/1
]).
:- use_module(goal_ir).

% By default we assume all unqualified calls live in module `user`.
compile_current_module(user).

directive_ir(ensure_loaded(Path), import(Path)) :- !.

directive_ir(Goal, ($direct):(IR)) :-
	goal_ir(block(direct), break($direct), Goal, IR).
