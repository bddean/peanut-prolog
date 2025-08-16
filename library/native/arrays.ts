import {
  termArgsArray,
  termTag,
  def_nondet,
  Var,
  unify_2,
  makeTerm,
  def_func,
} from "pl-runtime";

def_nondet("term_tag_args", 3, function*(
  Term,
  Tag,
  Args,
) {
  if (!(Term instanceof Var)) {
    const tag = termTag(Term);
    const args = termArgsArray(Term);

    for (const _ of unify_2(Tag, tag))
      yield* unify_2(Args, args);
  }
  if (Tag instanceof Var || Args instanceof Var) {
    throw new Error("insufficiently instantiated");
  }
  if (! (typeof Tag === "symbol" && Array.isArray(Args)))
    throw new Error("Bad type");

  const term = makeTerm(Tag, Args);
  yield* unify_2(term, Term);
})

// Note - ret undefined counts as failure i guess

for (const spec of `
  at3
  indexOf3 indexOf4
  lastIndexOf3 lastIndexOf4
  with4 concat3
  slice2 slice3 slice4
  pop2 unshift2
  join2
  toReversed2
  includes3
`);

for (const spec of `
  push2 shift2 reverse1
  fill2 fill3 fill4
`);

// Todo: splice (varargs)