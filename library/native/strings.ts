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
  return s.charCodeAt(i - 1);
});

// Peanut-specific function!
def_fun("get_string_char", 3, (s: Val, i: Val) => {
  if (typeof s !== "string") throw new Error("expected string");
  if (typeof i !== "number") throw new Error("expected number");
  if (i < 1 || i > s.length) throw new Error("out of bounds");
  return Symbol.for(s[i - 1]);
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

def_nondet("number_string", 2, function*(n: Val, s: Val) {
  if (n instanceof Var) {
    if (typeof s !== "string") throw new Error('bad type');
    return yield* unify_2(parseFloat(s), n);
  }
  // TODO: bignums!
  if (typeof n !== "number") throw new Error('bad type');
  return yield* unify_2(String(n), s);
});


// Useful reference:
// https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_45.html#SEC371
// https://web.archive.org/web/20240910004028/https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_45.html#SEC371
//
// https://www.swi-prolog.org/pldoc/man?section=syntax
//
// "v" flag instead of "u" flag. Maybe "u" is better fro compatibility?
const identchars = "[\\p{Letter}0-9_]*"
// TODO \uXXXX and \UXXXXXXXX
const esc = [
  `[btnvfreda]`,
  `x[a-fA-F0-9]{2}`, // Hex char code
  `[0-7]{1,3}`, // Octal char code
  `\\^\\?`, // delete
  `\\^[A-Za-z]`, // control characters
  `c\\s*`,
  `\\s`,
  `.`
].join("|");

const positiveNumber = [
  "0x[0-9a-fA-F]+",
  "0o[0-7]+",
  "0b[01]+",
  "(?:\\d*[.]\\d+|\\d+)(?:e-?\\d+)?",
].join("|");


const tokenzRe = new RegExp([
  `(?<quote_char>['"\`])(?<quotation>(?:\\\\(?:${esc})|(?!\\k<quote_char>)[^\\\\])*)\\k<quote_char>`,
  "(?<ws>(?:\\s|\\n)+)",
  "%(?<comment_line>.*$)",
  "(?:/[*](?<comment_block>(?:[^*]|[*][^\\/])*)[*]/)",
  `(?<number>-?${positiveNumber}+)`,
  `(?<ident>[\\p{Letter}_]${identchars})`,
  `(?<fullstop>[.])`,
  `(?<symbol_word>[\\p{Symbol}\\p{Punctuation}]+)`,
  "(?<solo>.)",
  "(?<ERROR>(.|\\n)*)",
].map(grp => `${grp}`).join("|"), "gmv");

export const tokenz = (s: string) => {
  console.log(tokenzRe);
  // return tokenzRe.exec(s);
  tokenzRe.lastIndex = 0;
  const toks: any[] = [];
  while (true) {
    const result = tokenzRe.exec(s);
    if (!result || !result[0] || !result.groups) break;
    toks.push(Object.fromEntries(
      Object.entries(result.groups).filter(
        ([, v]) => v !== undefined)));
  }
  return toks;
}
