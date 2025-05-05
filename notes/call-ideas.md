Ben's big idea note: In the first place we can implement call that *just*
invokes and doesn't support ,/2 and cut and control flow predicates.
This will help us get to self-hosting.

	ACTUALLY -- come to think of it, the generator stack idea probably
	works fine... let's just do that...

Dynamic call/N (+ cut) — implementation options
================================================

The goal: make `call/0 … call/8` work **exactly like in SWI-Prolog**, including
back-tracking and the behaviour of `!`, *for every back-end the compiler can
emit* (JS now; C, Rust, … later).

Below are three designs that *do* satisfy the tricky example

```prolog
call((writeln(show), ! ; writeln(hide))).
```

Any approach that fails this test (e.g. a simple registry lookup without an
interpreter) is deliberately **left out**.

---------------------------------------------------------------------
1  Runtime-level mini-interpreter (current JS back-end)
---------------------------------------------------------------------

Core idea
---------
Keep normal static compilation untouched, but add a **tiny generator-based
interpreter** in the runtime that is entered only for meta-calls.

Building blocks
---------------

1. **Predicate registry** (already useful for static cross-module calls):

   ```js
   const PRED = new Map<string,Function>();   // key = "module:fun/arity"

   export function reg(mod,name,arity,fn){
       PRED.set(`${mod}:${name}/${arity}`, fn);
   }
   ```

2. **`metaCall/1` generator** (≈20 LOC):

   * try registry lookup first; if found `yield* fn(...args)`;
   * otherwise recognise control constructs `,/2`, `;/2`, `->/2`, `!/0`,
     `true/0`, `fail/0` and interpret them directly;
   * undefined predicate ⇒ fail (as in Prolog).

3. **`call_1` … `call_8` wrappers** that (a) build a new term with the extra
   arguments (if any) and (b) `yield* metaCall`.

Cut semantics & tricky example
------------------------------
`!` is handled by returning from the current generator; this discards all
choice-points created *inside* the `metaCall` frame, leaving outer ones intact.
Consequently the example prints only `show`.

Tests
-----
* deterministic: `call(writeln(ok)).`
* non-det + cut: see `choose/1` example below.

Minimal host requirements
------------------------
* Generator support + `yield*` (already required by the runtime).
* The tiny `reg/…` and `metaCall` functions.

---------------------------------------------------------------------
2  Self-hosting JIT (compile goal on the fly)
---------------------------------------------------------------------

Core idea
---------
Once the compiler can run inside itself, treat `call(Goal)` as:

1. build IR for `Goal` at run-time,
2. lower it via the **current** back-end,
3. turn the emitted code into a real function, cache it, run it.

Uniform ABI
-----------

```c
typedef ChoiceGen* (*plts_funptr)(Val* args, int arity);

plts_funptr  plts_jit_compile(IR*);   // back-end specific
void         plts_jit_free(plts_funptr);
```

Back-end sketches
-----------------

* **JS**   `new Function(code)` → generator.
* **C / LLVM**   libgccjit / ORC JIT → function pointer.
* **Rust**   Cranelift or wasmtime JIT.
* **Python**   `exec` of generated `def`.
* **Guile / Racket**   `(compile …)` returns procedure.
* **WASM**   Instantiate extra module or fall back to interpreter.

Fallback & safety
-----------------
Targets that cannot implement `plts_jit_compile` reuse the interpreter from
approach 1; a build-time flag `--no-jit` can enforce that.

Cut semantics
-------------
Every JIT’d snippet is compiled with the same rules as normal predicates, so
`return;` still represents a cut. The tricky example therefore behaves
correctly.

---------------------------------------------------------------------
3  Mostly-in-IR: Prolog meta-interpreter compiled once
---------------------------------------------------------------------

Core idea
---------
Provide a *pure Prolog* definition of `meta_call/1..9` in `prelude.pl` and let
the existing compiler translate it into each target language. `call/N` in user
code is rewritten, during front-end normalisation, to `meta_call/N+1`.

Meta-interpreter sketch
-----------------------

```prolog
:- meta_predicate meta_call(0).

meta_call(G)             :- call(G).
meta_call(G, A1)         :- G =.. [F|As],
                            append(As,[A1], As1),
                            G2 =.. [F|As1],
                            call(G2).
… % extend mechanically up to 8

% Handles conjunction, disjunction, cut, etc., through ordinary Prolog code.
```

Host-side needs
---------------
* Same predicate registry as before.
* A single helper `invoke(Name,Arity,Args)` used by compiled `call/1`. That’s
  only ≈10 LOC and likely shared with normal inter-module calls.

Performance
-----------
Slightly slower than the JIT path (still acceptable for most code). Heavy
meta-programs can opt-in to the JIT when available.

Cut & tricky example
--------------------
Because the logic is written in Prolog and compiled just like any predicate,
cuts behave identically. The example again prints only `show`.

---------------------------------------------------------------------
4  Common semantics & test suite
---------------------------------------------------------------------

Unified behaviour to verify on **SWI** and on each compiled target:

```prolog
choose(a). choose(b).

t1 :- call(writeln(ok)).                        % determinate

t2 :- call((choose(X), !)), writeln(X),         % cut prunes second solution
      fail ; true.

t3 :- call((writeln(show), ! ; writeln(hide))). % tricky example

t4 :- ( call((member(Y,[x,y]), !))              % cut inside meta call
       ; writeln(outside) ).
```

Expected outputs:

```
ok
a
show
x
```

---------------------------------------------------------------------
5  Minimal runtime API (all approaches)
---------------------------------------------------------------------

1. `Var`, `Term`, `unify/2` — already present.
2. Predicate registry
3. `invoke/3` (or `metaCall/1` in JS variant).
4. Optional: `plts_jit_compile/1` for back-ends that support JIT.

With these primitives, any of the three working strategies can be layered on
top while preserving full Prolog semantics, including cut.

---------------------------------------------------------------------
6  Variant: “ancestral” cuts via an explicit choice-point stack
---------------------------------------------------------------------

Sometimes called the *WAM-style* implementation.  The idea is to make `!/0`
*compile* to the two low-level built-ins that SWI already exposes:

```prolog
!  ⟹  prolog_current_choice(CP), prolog_cut_to(CP).
```

Implementation sketch (JS)
-------------------------

1. Global stack `CP = []`, each entry = `{gen, active:true}`.
2. Helper `withChoice(gen)` wraps **every** predicate call:

   ```js
   function* withChoice(gen){
       CP.push({gen,active:true});
       try   { yield* gen; }
       finally { CP.pop(); }          // also runs when cut pops us
   }
   ```

3. The backend emits `for (const _ of withChoice(fn(...)))` instead of a raw
   `yield*`.  No annotation is required inside the generated predicate body
   itself.

4. Built-ins (≈15 LOC total):

   ```js
   function* prolog_current_choice(Ref){
       yield* unify_2(Ref, CP[CP.length-1]);
   }

   function* prolog_cut_to(Ref){
       while (CP.length && CP[CP.length-1] !== Ref) {
           const top = CP.pop();
           if (top.active && top.gen.return) top.gen.return();
       }
       yield;           // succeed once
   }
   ```

Performance cost vs. generator-`return` cut
------------------------------------------

* **Per predicate call:** one `push` on entry, one cheap `pop` on normal
  exit.  Roughly comparable to the bookkeeping the runtime already does for
  variables; <5 ns in Node 18.
* **Memory:** one array element per active choice-point (pointer + flag).
* **Cut complexity:** O(depth) because `prolog_cut_to/1` walks up the stack.
  Depth is typically small (≈ tens) and the loop does no allocations, so the
  cost is negligible in real programs.

Benefits
--------

1. Works for *any* goal, static or run-time data, because the stack is global
   and shared by the meta-interpreter.
2. Exposes `prolog_current_choice/1` & `prolog_cut_to/1` for user tooling and
   debugging, matching SWI.
3. Removes the need for the compiler to special-case `!` → `return;`—cleaner
   IR and identical semantics across back-ends.

Drawbacks
---------

* Constant overhead even when programs never use cuts.
* Slightly more complex runtime than the "return" solution (must manage a
  stack and guard against host exceptions).

When to pick it
---------------

• If you plan to share one runtime between multiple front-ends or want full
  compatibility with SWI’s debugging tools.
• If simplicity of the compiler (no special `!` transform) matters more than
  raw micro-benchmark speed.

---------------------------------------------------------------------
7  Future work & open questions
---------------------------------------------------------------------

* **Static peephole for ground goals** – When the argument of `call/N` is
  ground at compile-time, replace it with a direct predicate call to avoid the
  meta layer entirely.  This optimisation is backend-agnostic and does not
  require any runtime changes.

* **Cache for non-ground templates** – If the same *shape* of goal is invoked
  frequently with different variable bindings, memoise its term→registry key
  translation to reduce `=../2` churn.

* **Modules & qualification** – Extend the registry key to include a module
  qualifier (`foo:bar/2`) and update `meta_call`/`invoke` to honour
  `:/2` in run-time terms.

* **Thread safety** – Choice-point stack must be per-engine when true
  multi-threading arrives (web-workers, pthreads, etc.).  Straightforward: use
  one stack per worker.

* **Exception interaction** – Ensure that `throw/1` unwinds the stack just
  like `prolog_cut_to/1`.  This is a one-liner in the `finally` block of
  `withChoice` (already present in the sketch).

* **Determinism hints** – A future optimisation pass could mark predicates
  that are known det, so `withChoice` can be elided (zero overhead) for them.

* **Security policies** – JIT back-ends should obey content-security policy
  (CSP) in browsers and sandboxing on the server.  Interpreter fallback covers
  locked-down deployments.

* **Instrumentation hooks** – The explicit CP stack makes it easy to add
  debugging/tracing APIs (`spy/1`, `choice_stats/0`, …) later without touching
  the compiler.

  In practice each stack frame already stores `{gen, active}` so you can hang
  extra bookkeeping off it: timestamp, source-span, “retry” counter, etc.  A
  simple `plts_trace(on|off)` could toggle a hook that prints the stack on
  every `pushCP`/`popCP`.  Adding breakpoints means checking a predicate-ID
  against a hash-set right in `withChoice()` — O(1); the overhead is zero when
  tracing is disabled.
