g("my name is Cow,").
g("and wen its nite,").
g("or wen the moon") :- !.
g("is shiyning brite,").
g("and all the men").
g("haf gon to bed -").
g("i stay up late.").
g("i lik the bred.").

%% TODO This only passes by coincidence. Cuts are still totally broken.
% (We're inserting `break` in a way that causes unify() to fail to
% clean up, which just fails subsequent clauses...)
main :-
	g(X),
	writeln(X),
	fail.
main.
