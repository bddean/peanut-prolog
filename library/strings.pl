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
	In is I + 1,
	string_to_chars_(S, Cs, N, In).

string_chars(S, Cs) :- nonvar(S), !,
	string_length(S, N),
	string_to_chars_(S, Cs, N, 1).

string_chars(S, Cs) :- chars_to_string_(Cs, S).
