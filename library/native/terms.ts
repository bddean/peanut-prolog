import {
  CompoundTerm,
  db_set,
  deref,
  unify_2,
  Var,
  Val,
} from 'pl-runtime';

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

db_set("user:=../2", $003D$002E$002E_2);
