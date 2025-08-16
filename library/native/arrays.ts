import {
  termArgsArray,
  termTag,
  def_nondet,
  Var,
  unify_2,
  makeTerm,
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
