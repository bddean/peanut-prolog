:- ensure_loaded(library(native/primitives)).

atomic_to_string_(A, S) :- atom(A), !, atom_string(A, S).
atomic_to_string_(N, S) :- number(N), !, atom_string(N, S).
atomic_to_string_(S, S) :- string(S).

