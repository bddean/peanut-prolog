:- use_module("./comp").

:- begin_tests(comp_test).
test(compile_empty, nondet) :-
	clauses_ir([], IR),
	IR == nothing.

test(compile_simple_clause, nondet) :-
	clauses_ir([foo(X) :- bar(X)], IR),
	assertion(IR = defun(generator, $(foo, 1), _Body)).

test(statement_conjunction, nondet) :-
	goal_ir((a(1), b(2)), IR),
	assertion( IR = (StmtA *-> StmtB *-> yield) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(statement_disjunction, nondet) :-
	goal_ir((a(1) ; b(2)), IR),
	assertion( IR = (StmtA , StmtB) ),
	assertion( StmtA = funcall(_, _) ),
	assertion( StmtB = funcall(_, _) ).

test(term_call_ir) :-
	term_call_ir(writeln(hello), IR),
	IR = funcall(writeln, [_]).

test(compile_facts, [nondet]) :-
	clauses_ir([foo(1), foo(2), foo(3)], IR),
	assertion(IR = defun(generator, $(foo, 1), _Body)).

test(complex_predicate, nondet) :-
	Clauses = [
	    (foo(X, Y) :- bar(X), baz(Y)),
	    (foo(X, Y) :- qux(X, Y))
	],
	clauses_ir(Clauses, IR),
	IR = defun(generator, $(foo, 2), _).

:- end_tests(comp_test).
