% Binary tree pathfinding benchmark
% ---------------------------------
% A small synthetic benchmark that exercises unification and back-tracking
% but keeps list operations to a minimum.
%
% We build a complete binary tree of depth 5 using plain edge/2 facts
% (only ~30 facts so it stays tiny).  `path/3` explores the graph
% depth-first, producing a list that represents a route between two
% arbitrary nodes.
%
% The search space is large (every internal node has two children) but we
% ask for a *single* leaf, so the engine still needs to explore a fair
% amount of failing alternatives before it finds a solution.  That puts
% some load on choice-point creation and unification, which is exactly
% what we want to benchmark.
%
% The lists we do use (`[X|P]`) are short (≤ depth 5) and therefore do
% not dominate run-time, honouring the current limitation that list
% operations are unoptimised.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Binary tree                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Root is n0.

edge(n0, n1).  edge(n0, n2).

edge(n1, n3).  edge(n1, n4).
edge(n2, n5).  edge(n2, n6).

edge(n3, n7).  edge(n3, n8).
edge(n4, n9).  edge(n4, n10).
edge(n5, n11). edge(n5, n12).
edge(n6, n13). edge(n6, n14).

edge(n7, n15). edge(n7, n16).
edge(n8, n17). edge(n8, n18).
edge(n9, n19). edge(n9, n20).
edge(n10, n21). edge(n10, n22).
edge(n11, n23). edge(n11, n24).
edge(n12, n25). edge(n12, n26).
edge(n13, n27). edge(n13, n28).
edge(n14, n29). edge(n14, n30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Generic depth-first path finder                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% path(+From, +To, -Path)
% Path is a (small) list of nodes that starts with From and ends with To.

path(X, Y, [X, Y]) :-
	edge(X, Y).

path(X, Y, [X | Rest]) :-
	edge(X, Z),
	path(Z, Y, Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Benchmark driver                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We deliberately look for the *very last* leaf (n30) so that the search
% will have to back-track over most of the tree first.

main :-
	path(n0, n30, _).
