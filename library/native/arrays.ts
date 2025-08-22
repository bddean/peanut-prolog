import {
  termArgsArray,
  termTag,
  def_nondet,
  Var,
  unify_2,
  makeTerm,
  def_fun,
  def_det,
  deref,
} from "pl-runtime";

def_nondet("term_tag_args", 3, function* term_tag_args(
  Term,
  Tag,
  Args,
) {
  if (!(Term instanceof Var)) {
    const tag = termTag(Term);
    const args = termArgsArray(Term);

    for (const _ of unify_2(Tag, tag)) {
      yield* unify_2(Args, args);
    }
  }
  if (Tag instanceof Var || Args instanceof Var) {
    throw new Error("insufficiently instantiated");
  }
  if (!(typeof Tag === "symbol" && Array.isArray(Args)))
    throw new Error("Bad type");

  const term = makeTerm(Tag, Args);
  yield* unify_2(term, Term);
})

// Note - ret undefined counts as failure i guess

const splitSpecs = (s: string): [string, number][] => s
  .split(/\s+/m)
  .map(t => t.trim())
  .filter(Boolean)
  .flatMap((t, i) => {
    const items: [string, number][] = [];
    i = t.search(/\d/);
    const name = t.substring(0, i);
    const ars = t.substring(i);
    for (const ar of ars) {
      const n = parseInt(ar);
      items.push([name, n]);
    }
    return items;
  });

def_fun("array_length", 2, (a: any) => a.length);

for (const [name, n] of splitSpecs(`
  at3
  indexOf34
  lastIndexOf34
  with4 concat3
  slice234
  pop2 unshift2
  join3
  toReversed2
  includes3
`)) def_nondet(
  `array_${name}`, n,
  function*(a: any, ...args: any[]) {
    a = deref(a); // nosubmit...
    for (let i = 0 ; i < a.length ; i++) a[i] = deref(a[i]);
    for (let i = 0 ; i < args.length ; i++) args[i] = deref(args[i]);

    const ret = a[name](...args.slice(0, -1));
    // Treat undefined as failure for pop and unshift.
    if (ret === undefined) return;
    return yield* unify_2(args[args.length - 1], ret);
  }
);

for (const [name, n] of splitSpecs(`
  push2 shift2 reverse1
  fill2 fill3 fill4
`)) def_det(
  `array_${name}`, n,
  (a: any, ...args: any[]) => {
    a[name](...args)
  },
)

// Todo: splice (varargs)
