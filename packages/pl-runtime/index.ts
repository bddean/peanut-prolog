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

  toString() {
  	const val = deref(this);
  	if (typeof val === "symbol") return Symbol.keyFor(val);
  	if (val instanceof Var) return `_V${val.id}`;
  	return String(val);
	}
}

export const ArrayTag = Symbol.for("#");

export type CompoundTerm = Val[] | GenericCompoundTerm;

export const makeTerm = (tag: Atom, args: Val[]): CompoundTerm => {
  if (tag === ArrayTag) return args;
  return new GenericCompoundTerm(tag, args);
}

export const isCompound = (v: Val): v is CompoundTerm => Array.isArray(v) || v instanceof GenericCompoundTerm;

export const termTag = (T: Inst): symbol | string | bigint | number => {
  if (Array.isArray(T)) return ArrayTag;
  switch(typeof T) {
    case "symbol":
    case "number":
    case "bigint":
    case "string": return T;
    default: return T.tag;
  }
}

export const termArgsArray = (T: Inst): Val[] => {
  if (Array.isArray(T)) return T;
  switch(typeof T) {
    case "symbol":
    case "number":
    case "bigint":
    case "string": return [];
    default: return T.args;
  }
}

export class GenericCompoundTerm {
  constructor(
    public readonly tag: Atom,
    public readonly args: Val[],
  ) {}

  copy(vars = new Map<Var, Var>()): GenericCompoundTerm {
    return new GenericCompoundTerm(
      this.tag,
      this.args.map(A => {
        const X = deref(A);
        if (X instanceof GenericCompoundTerm) return X.copy(vars);
        if (!(X instanceof Var)) return X;
        if (!vars.has(X)) vars.set(X, new Var());
        return vars.get(X)!;
      })
    );
  }

  toString() {
    const tagName = Symbol.keyFor(this.tag);
    return `${tagName}(${this.args.map(String).join(", ")})`
  }
}

type UnboundVar = Var & { ref: UnboundSym };
type Atomic = Atom | string | number | bigint; // TODO... null??
type Atom = symbol;
type Inst = CompoundTerm | Atomic;
export type Val = Var | Inst;

export function deref(v: Val): Inst | UnboundVar {
  while (v instanceof Var && v.ref !== UnboundSym) v = v.ref;
  return v as UnboundVar;
}

export function* unify_2(A: Val, B: Val): Choices {
  A = deref(A); B = deref(B);
  if (A === B) return yield;
  if (A instanceof Var) return yield* A.set(B);
  else if (B instanceof Var) return yield* B.set(A);
  else if (Array.isArray(A) && Array.isArray(B)) {
    // Represented as #/n terms
    return yield* unifyArgs(A, B);
  }
  else if (A instanceof GenericCompoundTerm && B instanceof GenericCompoundTerm) {
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
  const val = deref(X);
  const s = typeof val === "symbol"  ? Symbol.keyFor(val) : String(val);
  console.log(s);
  yield;
}

export const fail_0 = function*() {}

// Registry of module objects for dynamic calls.
const moduleEvalFns = new Map<Atom, (s: string) => any>();
export const registerModule = (name: Atom, evalInModule: (js: string) => any) => {
  moduleEvalFns.set(name, evalInModule);
}

// Global predicate database for dynamic dispatch
const predicateDB = new Map<string, Function>();

export const db_set = (key: string, fn: Function) => {
  predicateDB.set(key, fn);
}

export const db_get = (key: string): Function | undefined => {
  return predicateDB.get(key);
}

const kv_db = new Map<symbol|string|number|bigint, Val>();

// TODO: {n,}b_deleteval and getval.
const nb_linkval = function*(K: Val, V: Val) {
  K = deref(K);
  switch(typeof K) {
    default:
      throw new Error("Invalid key type");
    case "string":
    case "number":
    case "bigint":
    case "symbol":
      break;
  }
  V = deref(V);
  kv_db.set(K, V);
  yield;
}
db_set("user:nb_linkval/2", nb_linkval);

const b_linkval = function*(K: Val, V: Val) {
  K = deref(K);
  const KTyped = K as symbol|string|number|bigint;
  const prev = kv_db.get(KTyped);
  try {
    yield* nb_linkval(K, V);
  } finally {
    if (prev !== undefined) kv_db.set(KTyped, prev!);
    else kv_db.delete(KTyped);
  }
}
db_set("user:b_linkval/2", b_linkval);

const SYM_COLON = Symbol.for(":");
const SYM_USER = Symbol.for("user");
const predWithMod = (T: Val): [Atom, Val] =>
  T instanceof GenericCompoundTerm && T.tag === SYM_COLON && T.args.length === 2
    ? T.args.map(deref) as [Atom, Val]
    : [SYM_USER, T];

export const call_1 = function(T: Val) {
	T = deref(T);
  const [mod, goal] = predWithMod(T);
  if (goal instanceof Var) {
    throw new Error("Can't call var.");
  }
  let name: symbol;
  let args: Val[];
  if (typeof goal === "symbol") { // atom
    name = goal;
    args = [];
  } else if (goal instanceof GenericCompoundTerm) {
    name = goal.tag;
    args = goal.args;
  } else throw new Error('nyi');

  const modName = Symbol.keyFor(mod) || "user";
  const predName = Symbol.keyFor(name);
  const key = `${modName}:${predName}/${args.length}`;
  const fn = db_get(key);
  if (!fn) {
    throw new Error(`Predicate not found: ${key}`);
  }
  return fn(...args);
}

// TODO: Consider compiling this in...
export const throw_1 = function*(Error: Val) {
  throw deref(Error);
}

// Initialize runtime predicates in the database
const true_0 = function*() { yield; };

db_set("user:unify/2", unify_2);
db_set("user:writeln/1", writeln_1);
db_set("user:fail/0", fail_0);
db_set("user:call/1", call_1);
db_set("user:throw/1", throw_1);
db_set("user:true/0", true_0);

export const def_nondet = (
	name: string,
	arity: number,
	pred: (...args: Val[]) => Choices
) => {
  db_set(`user:${name}/${arity}`, function*(...args: Val[]) {
    args = args.map(deref);
    yield* pred(...args);
  });
}

export const def_semidet = (
	name: string,
	arity: number,
	pred: (...args: Val[]) => boolean,
) => {
  db_set(`user:${name}/${arity}`, function*(...args: Val[]) {
    args = args.map(deref);
    if (pred(...args)) yield;
  });
}

export const def_det = (
	name: string,
	arity: number,
	pred: (...args: Val[]) => void,
) => {
  db_set(`user:${name}/${arity}`, function*(...args: Val[]) {
    args = args.map(deref);
    pred(...args)
    yield;
  });
}

export const def_fun = (
	name: string,
	arity: number,
	pred: (...args: Val[]) => Val,
) => {
  db_set(`user:${name}/${arity}`, function*(...args: Val[]) {
    args = args.map(deref);
    const inputs = args.slice(0, args.length - 1);
    const outputVar = args[args.length - 1];
    const returned = pred(...inputs)
    yield* unify_2(outputVar, returned);
  });
}

