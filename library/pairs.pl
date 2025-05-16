%:- module(pairs, []).
% This is just a stub for now to test import/export.

flip_pairs([], []).
flip_pairs([Key-Val|Pairs], [Val-Key|Flipped]) :-
    flip_pairs(Pairs, Flipped).

