import {
  db_set,
  deref,
  Val,
  Var,
} from 'pl-runtime';

const var_1 = function*(V: Val) {
  V = deref(V);
  if (V instanceof Var) yield;
}
db_set("user:var/1", var_1);

const nonvar_1 = function*(NV: Val) {
  NV = deref(NV);
  if (NV instanceof Var) return;
  yield;
}
db_set("user:nonvar/1", nonvar_1);

