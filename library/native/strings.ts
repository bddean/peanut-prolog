import { def_fun, def_nondet, Val, unify_2, Var } from "pl-runtime";

def_fun("string_length", 2, (s: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  return s.length;
});

//! get_string_code(+Index, +String, ?Code)
def_fun("get_string_code", 3, (s: Val, i: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  if (typeof i !== "number") throw new Error("expected number");
  if (i < 1 || i > s.length) throw new Error("out of bounds");
  return s.charCodeAt(i-1);
});

// Peanut-specific function!
def_fun("get_string_char", 3, (s: Val, i: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  if (typeof i !== "number") throw new Error("expected number");
  if (i < 1 || i > s.length) throw new Error("out of bounds");
  return Symbol.for(s[i-1]);
});

def_fun("string_concat", 3, (a: Val, b: Val) => {
  if (typeof a !== "string") throw new Error("expected string");
  if (typeof b !== "string") throw new Error("expected string");
  return a + b;
});

def_fun("string_upper", 2, (s: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  return s.toUpperCase();
});

def_fun("string_lower", 2, (s: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  return s.toLowerCase();
});

// NOTE: This is narrower then SWI version
def_nondet("atom_string", 2, function*(a: Val, s: Val) {
  if (a instanceof Var) {
    if (typeof s !== "string") throw new Error('bad type');
    return yield* unify_2(Symbol.for(s), a);
  }
  if (typeof a !== "symbol") throw new Error('bad type');
  const key = Symbol.keyFor(a);
  if (key === undefined) throw new Error('symbol is not registered!');
  return yield* unify_2(key, s);
});

def_nondet("string_codes_array", 2, function*(S: Val, CS: Val) {
  if (Array.isArray(CS)) return yield* unify_2(S, String.fromCodePoint(...CS as number[]))
  if (typeof S === "string") return yield* unify_2(
    CS,
    Array.from(S, (_, i) => S.codePointAt(i)!),
  );
  throw new Error('types!');
});
