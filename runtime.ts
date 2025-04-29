const UnboundSym = Symbol("Unbound Variable");
type UnboundSym = typeof UnboundSym;
type Choices = Generator<void, void, void>;
let _vid = 0; // For debugging etc.
export class Var {
	static *allocate() {
		while(true)	yield new Var();
	}
  ref: Val | UnboundSym = UnboundSym;
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

function* unify_2(A: Val, B: Val): Choices {
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
  for (const _ of unify_2(A[i], B[i])) {
    yield* unifyArgs(A, B, i + 1);
  }
}

export const writeln_1 = function*(X: Val) {
  console.log(deref(X));
  yield;
}

export const fail_0 = function*() {}
