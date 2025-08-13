import {
  db_set,
  deref,
  unify_2,
  Val,
} from 'pl-runtime';

function* succ_2(X: Val, Y: Val) {
  X = deref(X); Y = deref(Y);
  const tx = typeof X;
  const ty = typeof Y;
  // TODO or bigint...?
  if (tx === ty && tx === "number") {
    if ((X as number) + 1 === Y) yield;
  } else if (tx === "number") {
    yield* unify_2(Y, (X as number) + 1);
  } else if (ty === "number") {
    yield* unify_2(X, (Y as number) - 1);
  } else throw 'uninst';
}
db_set("user:succ/2", succ_2);

function* lt_2(X: Val, Y: Val) {
  X = deref(X); Y = deref(Y);
  if (typeof X !== "number" || typeof Y !== "number") throw 'typs';
  if(X < Y) yield;
}
db_set("user:</2", lt_2);

function* lte_2(X: Val, Y: Val) {
  X = deref(X); Y = deref(Y);
  if (typeof X !== "number" || typeof Y !== "number") throw 'typs';
  if(X <= Y) yield;
}
db_set("user:=</2", lte_2);

function* $is_op_4(Op: Val, R: Val, A: Val, B: Val) {
	Op = deref(Op);
	if (typeof Op !== 'symbol') return;
	A = deref(A);
	B = deref(B);
	// TODO: bigints??
	if (typeof A !== "number" || typeof B !== "number") return;
	switch(Op) {
	  default: throw new Error(`Unrecognized op: ${Symbol.keyFor(Op)}`);
	  case Symbol.for("+"): yield* unify_2(R, A + B);
	  case Symbol.for("-"): yield* unify_2(R, A - B);
	  case Symbol.for("*"): yield* unify_2(R, A * B);
	  case Symbol.for("/"): yield* unify_2(R, A / B);
	  case Symbol.for("<<"): yield* unify_2(R, A << B);
	  case Symbol.for(">>"): yield* unify_2(R, A >> B);
	}
}
db_set("$is_op/3", $is_op_4);

