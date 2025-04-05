const UnboundSym = Symbol("Unbound Variable");
type UnboundSym = typeof UnboundSym;
type Choices = Generator<void, void, void>;
let _vid = 0; // For debugging etc.
export class Var {
  ref: Val | UnboundSym = UnboundSym;;
  id = _vid++;
  *set(v: Val) {
    if (this.ref !== UnboundSym) return; // TODO skip this check for unify??
    this.ref = v;
    yield;
    this.ref = UnboundSym;
  }
}

let CURRENT_MODULE = "user";
export const Atom = (name: string) => Symbol.for(CURRENT_MODULE + ":" + name);

export class Term {
  constructor(
    public readonly tag: string,
    public readonly args: Val[],
  ) {}

  copy(vars = new Map<Var, Var>()): Term {
    return new Term(
      this.tag,
      this.args.map(A => {
        const X = deref(A);
        if (X instanceof Term) return X.copy(vars);
        if (!(X instanceof Var)) return X;
        if (!vars.has(X)) vars.set(X, new Var());
        return vars.get(X)!;
      })
    );
  }
}

type UnboundVar = Var & { ref: UnboundSym };
type Atomic = Atom | string | number | null | bigint; // TODO...
type Atom = string; // nosubmit...?
type Inst = Term | Atomic;
type Val = Var | Inst;

function deref(v: Val): Inst | UnboundVar {
  if (! (v instanceof Var)) return v satisfies Inst;
  if (v.ref === UnboundSym) return v as UnboundVar;
  return deref(v.ref);
}

function* unify(A: Val, B: Val): Choices {
  A = deref(A); B = deref(B);
  if (A === B) return yield;
  if (A instanceof Var) return yield* A.set(B);
  else if (B instanceof Var) return yield* B.set(A);
  else if (A instanceof Term && B instanceof Term) {
    if (
      A.tag !== B.tag
      || A.args.length !== B.args.length
    ) return;
    return yield* unifyArgs(A.args, B.args);
  }
  // Else, fail.
}

function* unifyArgs(A: Val[], B: Val[], i = 0): Choices {
  if (i >= A.length) return yield;
  for (const _ of unify(A[i], B[i])) {
    yield* unifyArgs(A, B, i + 1);
  }
}

type Builtin = (...args: Val[]) => Choices;
const db = new Map<Atomic, Map<number, (Inst|Builtin)[]>>();

export function* call(goal: Val): Choices {
  goal = deref(goal);
  if (goal instanceof Var) throw new Error("Can't call var.");
  const [tag, arity] = goal instanceof Term
    ? [goal.tag, goal.args.length]
    : [goal, 0];
  const entries = db.get(tag)?.get(arity);
  if (! entries) { throw new Error("Goal not found: " + goal)}
  // if (!entries) return;
  for (let entry of entries) {
    if (typeof entry === "function") {
      yield* entry(...(goal instanceof Term ? goal.args : []));
    } else if (entry instanceof Term) {
      entry = entry.copy();
      if (entry.tag === ":-" && entry.args.length === 2) {
        const [head, body] = entry.args;
        for (const _ of unify(goal, head)) {
          yield* call(body);
        }
      } else yield* unify(goal, entry)
    } // Else fail.
  }
}

// nosubmit asserta?
function* assertz(v: Val): Choices { }

export const add_fact = (tag: Atom, arity: number, Fact: Inst) => {
  if(!db.has(tag))db.set(tag, new Map());
  const byArity=db.get(tag)!;
  if(!byArity.has(arity))byArity.set(arity, []);
  byArity.get(arity)!.push(Fact);
}


export const run_goal = (G: Inst) => call(G).next();
const loadCtx = { add_fact, run_goal } as const;
// nosubmit hmm maybe move towards more of 1:1 mapping to js modules
// - atoms: just symbols these are the ONLY exports.
// - use_moduel --> import
// ... but we need to make sure it's serializable so use Symbol.for(mod:atom) etc
export const loadModule = (
  name: string,
  loader: (_: typeof loadCtx) => void
) => {
  CURRENT_MODULE = "name"
  try {
    loader(loadCtx);
  } finally {
    CURRENT_MODULE = "user";
  }
}


db.set("writeln", new Map([[1, [function*(arg: Val) {
  console.log(arg);
  yield;
}]]]));

db.set(",", new Map([[2, [function*(g1: Val, g2: Val) {
  for (const _ of call(g1)) yield* call(g2);
}]]]));

db.set("fail", new Map([[0, [function*() {}]]]));

// TODO call/n... though that can also just be def'd in prolog...
db.set("call", new Map([1].map(n => [n, [call]])));
