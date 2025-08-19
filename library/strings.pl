:- ensure_loaded(library(ranges)).
:- ensure_loaded(library(math)).

:- ensure_loaded(library(native/strings)).

%! string_code(?Index, +String, ?Code)
string_code(I, S, C) :-
	string_length(S, N),
	between(1, N, I),
	get_string_code(I, S, C).
