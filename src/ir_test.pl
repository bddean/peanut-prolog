:- use_module("./ir").
:- begin_tests(ir_test).
test_tform(T0, T) :- number(T0), !, T is T0 + 1000.
test_tform(T0, T) :- is_list(T0), !, append(T0, [seen], T).
test_tform(T0, T) :-
	T0 =.. [A|Xs0],
	append(Xs0, [seen], Xs),
	T =.. [A|Xs].

test(basic, [nondet]) :-
	walk_ir(test_tform, (yield, yield), W),
	assertion(W == ','(yield(seen), yield(seen), seen)).

test(list_args, [nondet]) :-
	walk_ir(
		test_tform,
		allocate_vars([$.(3),$.(b)]),
		Vs
	),
	assertion(Vs == allocate_vars([
		$.(1003, seen),
		$.(b(seen), seen)
	], seen)).

test(simple_defun, [nondet]) :-
	IR0 = defun(generator, $(asdf, 1), (
		yield
	)),
	walk_ir(test_tform, IR0, IR),
	assertion(defun(generator, $(asdf, 1, seen), (
		yield(seen)
	), seen) == IR).

test(real_defun, [nondet]) :-
	IR0 = defun(generator,(=)/2,(
		allocate_vars([$.(0),$.(1)]),
		(funcall(unify,[$.('CALLED_TERM'),\('$VAR'(A)='$VAR'(A))])
		*->funcall(true,[])
		*->yield
		),nothing
	)),
	walk_ir(test_tform, IR0, IR),
	assertion(IR \= IR0).

test(real_alloc, [nondet]) :-
	IR0 = allocate_vars([$.(0),$.(1)]),
	walk_ir(test_tform, IR0, IR),
	assertion(IR \= IR0).

:- end_tests(ir_test).
