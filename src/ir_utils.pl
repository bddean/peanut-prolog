:- module(ir_utils, [
    % walk_ir/3 is a DCG that walks an arbitrary IR term and applies a
    % transformation predicate to every node (bottom-up, single pass).
    % The first argument is a goal G/3 (in the same signature
    % as this DCG) that transforms one node into another; it can fail to
    % leave the node unchanged.
    %
    % Example
    %   ?- phrase(walk_ir(example_tform), In, Out).
    walk_ir/3
]).

:- meta_predicate walk_ir(2, ?, ?).

% walk_ir(+Tform)//
% -----------------
% Generic n-ary tree traversal for the in-memory IR used by the front-end.
% The traversal is *bottom-up* and *single-pass*: first the current node is
% offered to the transformation, afterwards the children are visited.

walk_ir(G) --> walk_kids(G), tform_node(G).

tform_node(G, E0, E) :-
    (   call(G, E0, E)
    *-> true
    ;   E = E0
    ).

walk_kids(G, E0, E) :-
    (   walk_kids_(G, E0, E)
    *-> true
    ;   E = E0
    ).

% Child traversal for every IR constructor we currently emit.
% Keep this in sync with new IR nodes – this file is the *single source of
% truth* for the tree structure.
%
% TODO: Generate these at a higher lvl of abstraction.

walk_kids_(G, E0, E) :-
    member(E0-E, [
        (M0, N0)    - (M, N),
        (M0 -> N0)  - (M -> N),
        (M0 *-> N0) - (M *-> N),
        (M0 := N0)  - (M := N)
    ]),
    maplist(walk_ir(G), [M0, N0], [M, N]).

walk_kids_(G, yield_all(X0), yield_all(X)) :-
    walk_ir(G, X0, X).

walk_kids_(G, defun(Type, Name, Body0), defun(Type, Name, Body)) :-
    walk_ir(G, Body0, Body).

walk_kids_(G, funcall(Name, Args0), funcall(Name, Args)) :-
    maplist(walk_ir(G), Args0, Args).

walk_kids_(G, allocate_vars(Names0), allocate_vars(Names)) :-
    maplist(walk_ir(G), Names0, Names).

walk_kids_(G, L0:B0, L:B) :-
    maplist(walk_ir(G), [L0, B0], [L, B]).

walk_kids_(G, break(L0), break(L)) :-
    call(G, L0, L).
