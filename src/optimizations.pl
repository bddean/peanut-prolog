:- module(optimizations, [
	erase_true/2,
	loop_to_yield_all/2
]).

erase_true((funcall(true, []) *-> W), W).
loop_to_yield_all((G *-> yield), yield_all(G)).
