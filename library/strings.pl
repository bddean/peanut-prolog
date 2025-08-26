:- ensure_loaded(library(ranges)).
:- ensure_loaded(library(math)).
:- ensure_loaded(library(arrays)).

:- ensure_loaded(library(native/strings)).

%! string_code(?Index, +String, ?Code)
string_code(I, S, C) :-
	string_length(S, N),
	between(1, N, I),
	get_string_code(S, I, C).

string_codes(S, Cs) :-
	nonvar(S) ->
		string_codes_array(S, Arr),
		array_list(Arr, Cs)
	; array_list(Arr, Cs),
		string_codes_array(S, Arr).

chars_to_string_(Cs, S) :-
	maplist(atom_string, Cs, Subs),
	array_list(Arr, Subs),
	array_join(Arr, "", S).

string_to_chars_(S, [], N, I) :- I > N, !.
string_to_chars_(S, [C|Cs], N, I) :- I =< N,
	get_string_char(S, I, C),
	succ(I, In),
	string_to_chars_(S, Cs, N, In).

string_chars(S, Cs) :- nonvar(S), !,
	string_length(S, N),
	string_to_chars_(S, Cs, N, 1).

string_chars(S, Cs) :- chars_to_string_(Cs, S).

atomics_to_string(Ats, S) :- atomics_to_string(Ats, "", S).
atomics_to_string(Ats, Sep, S) :-
	maplist(atomic_string_, Ats, Ss),
	%% Hm.
	%% - More efficient?: Use array_push directly
	%% - More idiomatic?: foldl(string_join)
	array_list(Sz, Ss),
	atomic_string_(Sep, SSep),
	array_join(Sz, SSep, S).

atomic_string_(S, S) :- string(S), !.
atomic_string_(A, S) :- atom(A), !, atom_string(A, S).
atomic_string_(N, S) :- number(N), !, number_string(N, S).
atomic_string_(_, _) :- throw(type_error).
