
import {
  db_set,
  deref,
  unify_2,
  Val,
} from 'pl-runtime';
let counter = 0;

function* gensym_2(Prefix: Val, Result: Val) {
  Prefix = deref(Prefix);
  if (typeof Prefix !== "string") throw 'need string';
  const result = Prefix + "_" + (counter++).toString(16);
  yield* unify_2(result, Result);
}
db_set("user:gensym_2", gensym_2);
