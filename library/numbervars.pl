:- ensure_loaded(library(lists)).

numbervars(T, N0, N) :-	numbervars(T, N0, N, []).

numbervars(T, N0, N, Opts) :-
  term_variables(T, Vs),
	length(Vs, Len),
	N1 is N0 + Len - 1,
  numlist(N0, N1, Ns),
	numbervars__opts_(Opts, FunctorName),
  maplist(numbervars__var_(FunctorName), Ns, Vs),
	N is N1 + 1.

numbervars__var_(FunctorName, N, Wrapped) :-
	Wrapped =.. [FunctorName, N].

numbervars__opts_(
	Opts,
	FunctorName
	%% AttVar -- not supported yet
	%% singletons -- not supported yet
) :-
	append(Opts, [functor_name('$VAR')], OptsWithDefaults),
  memberchk(functor_name(FunctorName), OptsWithDefaults).

