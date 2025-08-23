import {
  db_set,
  deref,
  unify_2,
  Val,Var,
  isCompound,
  termTag,
  termArgsArray,
  def_fun,
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

const allTermVars = (X: Val): Var[] =>
  X instanceof Var ? [X]
  : typeof X != "object" ? []
  : termArgsArray(X).map(deref).flatMap(allTermVars);

const term_variablez = (X: Val) => [...new Set(allTermVars(X))];

def_fun("term_variablez", 2, term_variablez);
