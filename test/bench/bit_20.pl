% generate all 20-length bit sequences (2^20 solutions) using simple facts
bit(0).
bit(1).

bits(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
    bit(A), bit(B), bit(C), bit(D), bit(E),
    bit(F), bit(G), bit(H), bit(I), bit(J),
    bit(K), bit(L), bit(M), bit(N), bit(O),
    bit(P), bit(Q), bit(R), bit(S), bit(T).

main :- bits(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), fail; true.
