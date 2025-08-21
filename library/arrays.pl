:- ensure_loaded(library(native/arrays)).

array_list(Arr, Ls) :- Arr =.. [#|Ls].
