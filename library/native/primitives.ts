import {
  Var,
  unify_2,
  def_nondet,
} from "pl-runtime";


def_nondet("atom_string", 2, function*(Atom, Str) {
  if (Atom instanceof Var) {
    if (Str instanceof Var) return;
    if (typeof Str !== "string") return;
    return yield* unify_2(Atom, Symbol.for(Str));
  } else if (Str instanceof Var) {
    if (typeof Atom !== "symbol") return;
    const key = Symbol.keyFor(Atom);
    if (key === undefined) return;
    return yield* unify_2(key, Str);
  } else {
    if (typeof Str !== "string") return;
    if (typeof Atom !== "symbol") return;
    if (Atom !== Symbol.for(Str)) return;
    yield;
  }
});

def_nondet("number_string", 2, function*(Num, Str) {
  if (Num instanceof Var) {
    if (Str instanceof Var) return;
    if (typeof Str !== "string") return;
    return yield* unify_2(Num, parseFloat(Str));
  } else if (Str instanceof Var) {
    if (typeof Num !== "number") return; // TODO: bigints?
    return yield* unify_2(Num+"", Str);
  } else {
    if (typeof Str !== "string") return;
    if (typeof Num !== "number") return;
    if (Num+"" !== Str) return;
    yield;
  }
});
