main :-
	var(_),
	A = 1,
	\+ var(A),

	nonvar(A),
	nonvar(hello(_)),

	number(A),
	number(1),
	number(0.5),

	string("asdfasfasdf"),
	\+string(asdfasfasdf),

	atom(asdfasfasdf),
	atom(''),
	atom('_'),
	atom('Capitalized'),
	atom('With Spaces'),

	atomic("str"),
	atomic(at),
	atomic([]),
	atomic(1200),
	\+atomic(_),
	\+atomic([1,2]),

	writeln(ok).
