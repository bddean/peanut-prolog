g("my name is Cow,").
g("and wen its nite,").
g("or wen the moon").
g("is shiyning brite,").
g("and all the men").
g("haf gon to bed -").
g("i stay up late.").
g("i lik the bred.").

loop :-
	g(X),
	writeln(X),
	fail.
loop.

:- loop.
