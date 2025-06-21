
main :-
	Alpha = alpha(a,b,c,d,e,f,g,h,i,j),
	between(1, 10, X),
	writeln(X),
	arg(X, Alpha, Char),
	writeln(Char),
	fail.
main.

:- main.
