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

export class CompoundTerm {
  constructor(
    public readonly tag: Atom,
    public readonly args: Val[],
  ) {}

  copy(vars = new Map<Var, Var>()): CompoundTerm {
    return new CompoundTerm(
      this.tag,
      this.args.map(A => {
        const X = deref(A);
        if (X instanceof CompoundTerm) return X.copy(vars);
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
type Atomic = Atom | string | number | null | bigint; // TODO...
type Atom = symbol;
type Inst = CompoundTerm | Atomic;
type Val = Var | Inst;

function deref(v: Val): Inst | UnboundVar {
  while (v instanceof Var && v.ref !== UnboundSym) v = v.ref;
  return v as UnboundVar;
}

export function* unify_2(A: Val, B: Val): Choices {
  A = deref(A); B = deref(B);
  if (A === B) return yield;
  if (A instanceof Var) return yield* A.set(B);
  else if (B instanceof Var) return yield* B.set(A);
  else if (A instanceof CompoundTerm && B instanceof CompoundTerm) {
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

const var_1 = function*(V: Val) {
  V = deref(V);
  if (V instanceof Var) yield;
}
db_set("user:var/1" ,var_1);

const get_arg$_3 = function*(Index: Val, T: Val, Element: Val) {
	Index = deref(Index);
	T = deref(T);
	Element = deref(Element);
	if (! (T instanceof CompoundTerm) || ! (typeof Index === "number")) throw new Error('bad type');
	yield* unify_2(T.args[Index], Element);
}
db_set("user:get_arg$/3", get_arg$_3);

const functor_3 = function*(T: Val, Name: Val, Arity: Val) {
	T = deref(T);
	Name = deref(Name);
	Arity = deref(Arity);
	if (! (T instanceof CompoundTerm)) throw 'typ';
	for (const _ of unify_2(Name, T.tag))
		yield* unify_2(Arity, T.args.length);
}
db_set("user:functor/3", functor_3);

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

// NEXT: Between?

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
  T instanceof CompoundTerm && T.tag === SYM_COLON && T.args.length === 2
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
  } else if (goal instanceof CompoundTerm) {
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

const SYM_LIST = Symbol.for("[|]");

// =../2
export const $003D$002E$002E_2 = function*(T: Val, List: Val) {
	T = deref(T); List=deref(List);
	if (typeof T === "symbol") {
	  return yield* unify_2(List, new CompoundTerm(SYM_LIST, [T, SYM_LIST]));
	}
	if (! (T instanceof Var)) {
    if (! (T instanceof CompoundTerm)) throw new Error('bad type');
    const argsLs = T.args.reduceRight(
      (acc, Arg) => new CompoundTerm(SYM_LIST, [Arg, acc]),
      SYM_LIST,
    );
    const ConstructedList = new CompoundTerm(SYM_LIST, [T.tag, argsLs]);
    return yield* unify_2(ConstructedList, List);
	}
	if (List instanceof CompoundTerm) {
	  let [tag, argsLs] = List.args.map(deref) as [Val, Val];
	  if (typeof tag !== "symbol") throw new Error('bad type ' + typeof tag);
	  let args: Val[] = [];
	  while (argsLs !== SYM_LIST) {
	    const t = deref(argsLs) as CompoundTerm;
	    if (t instanceof Var) throw new Error('uninst');
	  	args.push(t.args[0]);
	    argsLs = t.args[1];
	  }
	  if (args.length === 0) {
	    return yield* unify_2(T, tag);
	  }
	  return yield* unify_2(T, new CompoundTerm(tag, args));
	}
  throw new Error('uninst');
}

// Initialize runtime predicates in the database
const true_0 = function*() { yield; };

db_set("user:unify/2", unify_2);
db_set("user:writeln/1", writeln_1);
db_set("user:fail/0", fail_0);
db_set("user:call/1", call_1);
db_set("user:throw/1", throw_1);
db_set("user:=../2", $003D$002E$002E_2);
db_set("user:true/0", true_0);
