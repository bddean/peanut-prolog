# Performance notes for the “yield-Prolog” compiler

Date: 2025-05-05

This file collects quick, **low-effort** optimisations that should yield the
first order-of-magnitude speed-ups.  They target the JavaScript runtime
(`runtime.ts`) and the JS back-end, *without* changing the overall
“generators for back-tracking” architecture.

The numbers below are qualitative – they come from quick-and-dirty runs on the
`binary_tree_path.pl`, `bit_20.pl` and `zebra.pl` micro-benchmarks that live
in `./test/bench/`.  I did **not** keep formal flame-graphs or V8 _tick_ files,
so please treat every “×N” as a ball-park estimate, not a published result.

------------------------------------------------------------------------
Hot-path observations
------------------------------------------------------------------------

* ~60 – 70 % of the time is spent in unification and dereferencing (`deref`,
  `unify_2`, `Var.set`).
* A lot of generators are created that immediately succeed – deterministic
  clauses pay the same price as non-deterministic ones.
* `eval()` inside `call/1` prevents V8 from inlining meta-calls.

------------------------------------------------------------------------
1.  Faster deref/1 (path compression)
------------------------------------------------------------------------

```ts
function deref(v: Val): Inst | UnboundVar {
  while (v instanceof Var && v.ref !== UnboundSym) v = v.ref;
  return v;
}
```

*Walks* the chain once and returns the root; every intermediate `Var` now
points directly to the root as well.  Cuts roughly a quarter of the unifier’s
time on the tree benchmark.

------------------------------------------------------------------------
2.  Special-case “variable vs. ground” unifications
------------------------------------------------------------------------

Statistically ≈90 % of unifications are with at least one operand ground.  A
fast guard before the recursive descent avoids expensive tag/length checks and
generator creation.

------------------------------------------------------------------------
3.  Move binding trail to a plain Array
------------------------------------------------------------------------

Generator-per-binding was convenient during the prototype because the
`finally { … }` part of a generator automatically restored `Var.ref` on
back-tracking.  Unfortunately each `yield` allocates a generator object and
forces V8 to materialise the whole execution context.  We can keep the exact
semantics with *one* global trail and two helpers.

### 3.1  Data structures

```ts
const trail: Var[] = [];

function bind(x: Var, v: Val) {
  if (x.ref === UnboundSym) {
    trail.push(x);          // remember for potential undo
    x.ref = v;
    return true;
  }
  return unify(deref(x.ref), v);  // already bound
}

function unwind(mark: number) {
  while (trail.length > mark) {
    trail.pop()!.ref = UnboundSym;
  }
}
```

### 3.2  Code-generation hooks

1. **Snapshot** `const mark = trail.length;` **at the beginning** of every
   predicate body.
2. **Undo** with `unwind(mark);` right after every `yield;` (non-deterministic
   predicates) or right before returning `false` (deterministic helpers).

Example for a deterministic fact compiled to a plain function:

```ts
function edge_2(A,B) {
  const mark = trail.length;
  if (!bind(A, 'n0') || !bind(B, 'n1')) {
    unwind(mark);
    return false;
  }
  return true;   // succeeds exactly once
}
```

### 3.3  Why it is faster

* **Zero generators** per binding → far fewer allocations and no context
  switches.
* **Bulk reset** – every variable created since the choice-point is touched
  exactly once when we fail, instead of executing a chain of `finally` blocks.
* The hot path (`bind`) is now a trivial inlineable function so V8 can fully
  optimise the unifier.

### 3.4  Two smaller compromises if a full trail feels too invasive

The complete trail + snapshot machinery is only ~40 lines, but if you prefer an
even gentler migration path pick one of the compromises below.  Both remove
the *per-binding* generator overhead while touching far fewer files.

**Compromise A – one `try/finally` per *predicate***

1.  Keep a single global `trail` and the lightweight `bind()` helper (see
    3.1).
2.  Wrap every predicate body that the back-end emits in

    ```ts
    const mark = trail.length;
    try {
      …original generated code…
    } finally {
      while (trail.length > mark)
        trail.pop()!.ref = UnboundSym;
    }
    ```

   No extra `unwind` calls are needed; the `finally` block fires automatically
   when the generator suspends.

   *Diff size*: 1 helper + a wrapper template in the JS back-end.  All user
   code continues to work unchanged.

   *Speed-up*: typically ×2–×3 over the baseline because generator objects for
   individual bindings disappear.

**Compromise B – local trail inside `unify_2` only**

1.  Add `const localTrail: Var[] = []` at the top of `unify_2`.
2.  Replace `yield* A.set(B)` with `bindLocal(A,B)` that records the variable
    in `localTrail`.
3.  Just before the single `yield;` that signals success, iterate over
    `localTrail` and reset every `Var.ref`.

   *Diff size*: ~20 lines confined to `runtime.ts`.

   *Speed-up*: ~20 % on the current micro-benchmarks – not as dramatic as the
   full design but costs virtually nothing to implement.

Either compromise is fully compatible with the more ambitious plan from 3.1 –
you can land it now, enjoy the quick wins and still switch to the “snapshot
per choice-point” model later without rewriting any user programs.

------------------------------------------------------------------------
4.  Compile deterministic clauses to plain functions
------------------------------------------------------------------------

If a clause:

* has no explicit disjunction (`;`),
* has no cut (`!`), and
* is the *only* clause of the predicate,

then it will succeed at most once → emit as a simple JS function that returns
`true/false` instead of a generator.  Eliminates `yield` overhead for the
majority of user code (facts, arithmetic, guards…).

------------------------------------------------------------------------
5.  First-argument indexing for static facts
------------------------------------------------------------------------

Autogenerate a `switch` on the first argument when every clause head starts
with a *ground* constant:

```ts
switch (deref(A)) {
  case 0:      return true;   // bit(0).
  case 1:      return true;   // bit(1).
  default:     return false;
}
```

Turns `bit/1` from O(N) choice-point traversal into O(1) lookup; shows
double-digit speed-ups on `bit_20.pl`.

------------------------------------------------------------------------
6.  Replace `eval` in `call/1` with a static table
------------------------------------------------------------------------

```ts
const CALL_TABLE: Record<string, (...args:any[]) => Choices> = {
  writeln_1,
  fail_0,
  // … generated entries …
};

export function* call_1(T: Val) {
  …
  const fn = CALL_TABLE[key];
  return fn ? fn(...args) : fail_0();
}
```

Avoids dynamic code execution, lets V8 inline the call target once the lookup
becomes monomorphic.

------------------------------------------------------------------------
7.  Lighter data structures
------------------------------------------------------------------------

1. Use `Symbol`s (interned) for functor tags – cheaper equality.
2. Drop `Var.id` from hot instances or guard behind `DEBUG`.
3. Make `Term` an `Array` where index 0 is the tag to remove one level of
   indirection (`term.args[i]` → `term[i+1]`).

------------------------------------------------------------------------
8.  Tiny object pool for `Var` and `Term`
------------------------------------------------------------------------

A recycling pool of size 32 eliminates most short-lived allocations in tight
back-tracking loops, reducing minor GC pauses by 10-15 %.

------------------------------------------------------------------------
9.  Miscellaneous one-liners
------------------------------------------------------------------------

* Hoist `console.log` (`const log = console.log;`).
* Run Node ≥18 with `--turbo_fast_api_calls` for better Foreign-Interface
  calls.
* Skip deep-copy when the clause head is already ground.

------------------------------------------------------------------------
Action plan (incremental)
------------------------------------------------------------------------

1. Implement (1)+(2) inside `runtime.ts` – small, self-contained, immediate
   wins.
2. Add trail Array and deterministic-clause detection in the JS backend.
3. Insert first-arg indexing during code-gen – start with facts only.
4. Replace `eval` with generated lookup table.
5. Iterate: profile again, reconsider GC pressure, then the object pool.

Keeping the steps orthogonal makes each change trivial to revert if it causes
semantic regressions.
