function* =_2(X) {
const [_0, _1] = Var.allocate();
;yield* unify(X, new Term("=", [_0, _0]));
;
}function* member_2(X) {
const [_0, _1, _2, _3, _4, _5] = Var.allocate();
;yield* unify(X, new Term("member", [_0, new Term("[|]", [_0, _1])]));
;for (const _ of unify(X, new Term("member", [_2, new Term("[|]", [_3, _4])]))) {
yield* member(_2, _4);

};
}function* append_3(X) {
const [_0, _1, _2, _3, _4, _5] = Var.allocate();
;yield* unify(X, new Term("append", [[|], _0, _0]));
;for (const _ of unify(X, new Term("append", [new Term("[|]", [_1, _2]), _3, new Term("[|]", [_1, _4])]))) {
yield* append(_2, _3, _4);

};
}