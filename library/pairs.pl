:- module(pairs, [pairs_keys/2]).
% This is just a stub for now to test import/export.

pairs_keys([], []).
pairs_keys([K-_|Ps], [K|Ks]) :- pairs_keys(Ps, Ks).
