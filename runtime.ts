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
    try {
    	this.ref = v;
    	yield;
    } finally {
    	this.ref = UnboundSym;
    }
  }
}

// TODO: These lines are unused.
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
  while (v instanceof Var && v.ref !== UnboundSym) v = v.ref;
  return v as UnboundVar;
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

export const call_1 = function(T: Val) {
	T = deref(T);
  if (T instanceof Var) {
    throw new Error("Can't call var.");
  }
  let name: string;
  let args: Val[];
  if (typeof T === "string") { // atom
    name = T;
    args = [];
  } else if (T instanceof Term) {
    name = T.tag;
    args = T.args;
  } else throw new Error('nyi');
  const unescaped = `${name}_${args.length}`;
  const ident = unescaped.replace(/(^[0-9])|[^A-Za-z0-9_]/g, char => {
    const code = char.charCodeAt(0);
    const zeroes = "0000";
    const hex = code.toString(16);
    const padded = zeroes.substring(0, 4 - hex.length) + hex;
    return "$" + padded;
  });
  const fn = eval(ident);
  return fn(...args);
}

// TODO: Consider compiling this in...
export const throw_1 = function*(Error: Val) {
  throw deref(Error);
}

// =../2
export const $003D$002E$002E_2 = function*(T: Val, List: Val) {
	T = deref(T); List=deref(List);
	if (typeof T === "string") {
	  return yield* unify_2(List, new Term("[|]", [T, "[|]"]));
	}
	if (! (T instanceof Var)) {
    if (! (T instanceof Term)) throw new Error('bad type');
    const argsLs = T.args.reduceRight(
      (acc, Arg) => new Term("[|]", [Arg, acc]),
      '[|]',
    );
    const ConstructedList = new Term('[|]', [T.tag, argsLs]);
    return yield* unify_2(ConstructedList, List);
	}
	if (List instanceof Term) {
	  let [tag, argsLs] = List.args.map(deref) as [Val, Val];
	  if (typeof tag !== "string") throw new Error('bad type ' + typeof tag);
	  let args: Val[] = [];
	  while (argsLs !== "[|]") {
	    const t = deref(argsLs) as Term;
	    if (t instanceof Var) throw new Error('uninst');
	  	args.push(t.args[0]);
	    argsLs = t.args[1];
	  }
	  if (args.length === 0) {
	    return yield* unify_2(T, tag);
	  }
	  return yield* unify_2(T, new Term(tag, args));
	}
  throw new Error('uninst');
}
