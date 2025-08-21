import {
  CompoundTerm,
  GenericCompoundTerm,
  db_set,
  deref,
  unify_2,
  Var,
  Val,
  isCompound,
  termTag,
  termArgsArray,
  makeTerm,
} from 'pl-runtime';

const get_arg$_3 = function*(Index: Val, T: Val, Element: Val) {
	Index = deref(Index);
	T = deref(T);
	Element = deref(Element);
	if (typeof Index !== "number" || !isCompound(T)) {
	  throw new Error('bad type ')
	}
	const args = termArgsArray(T);
	yield* unify_2(args[Index], Element);
}
db_set("user:get_arg$/3", get_arg$_3);

// TODO these should work with ~all atomics

const functor_3 = function*(T: Val, Name: Val, Arity: Val) {
	T = deref(T);
	Name = deref(Name);
	Arity = deref(Arity);
	if (! (isCompound(T))) throw 'typ';
	for (const _ of unify_2(Name, termTag(T)))
		yield* unify_2(Arity, termArgsArray(T).length);
}
db_set("user:functor/3", functor_3);

const SYM_LIST = Symbol.for("[|]");

// =../2
export const $003D$002E$002E_2 = function*(T: Val, List: Val) {
	T = deref(T); List=deref(List);
	if (typeof T === "symbol") { // Uhhh are u sure this is right...
	  return yield* unify_2(List, makeTerm(SYM_LIST, [T, SYM_LIST]));
	}
	if (! (T instanceof Var)) {
    if (! isCompound(T)) throw new Error('bad type');
    const args = termArgsArray(T);
    const argsLs = args.reduceRight(
      (acc, Arg) => makeTerm(SYM_LIST, [Arg, acc]),
      SYM_LIST,
    );
    const ConstructedList = makeTerm(SYM_LIST, [termTag(T), argsLs]);
    return yield* unify_2(ConstructedList, List);
	}
	// T is a variable, List should be a proper list
	if (List instanceof GenericCompoundTerm) {
	  // TODO check if is actual list
	  let [tag, argsLs] = List.args.map(deref) as [Val, Val];
	  if (typeof tag !== "symbol") throw new Error('bad type ' + typeof tag);
	  let args: Val[] = [];
	  while (argsLs !== SYM_LIST) {
	    const t = deref(argsLs) as CompoundTerm;
	    if (t instanceof Var) throw new Error('uninst');
	    const subArgs = termArgsArray(t);
	  	args.push(subArgs[0]);
	    argsLs = subArgs[1];
	  }
	  if (args.length === 0) {
	    return yield* unify_2(T, tag);
	  }
	  return yield* unify_2(T, makeTerm(tag, args));
	}
  throw new Error('uninst');
}

db_set("user:=../2", $003D$002E$002E_2);
