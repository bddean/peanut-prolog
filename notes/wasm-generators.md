# WASM backend – generator strategy

Early draft, focussed on how back-tracking generators ( *yield*/yield_all) map to
WebAssembly’s “no coroutines” execution model.  The rest of the backend can be
reasonably straightforward function calls; generators are the only *control
flow* feature that needs extra machinery.

## 1. Goals / non-goals

* Keep the **public JS API** identical to the current runtime so integration
  tests stay unchanged: a predicate compiled in *generator mode* is surfaced as
  an object with a `next()` method returning `{done:Bool, value:Term}`.
* MVP: support deterministic & nondet predicates, cut (!), yield/yield_all.
* Eventually remove all JS shims, but first version can keep unify/Term on the
  JS side so we only worry about control flow.

## 2. Conceptual model

1 Prolog clause → *state machine* with one entry + many re-entry points.

Each time a clause “yields” an answer, we snapshot **where** to resume and push
it on a *choice-point stack*.  Traditional WAM does this on the heap; we’ll do
the same but store it in Wasm linear memory.

```
struct ChoicePoint {
  u32  pc;           // label to jump back to
  u32  fp;           // frame pointer of the clause where the cp was created
  u32  env_off;      // offset of env snapshot (logic vars etc.)
}

// “stack” grows upward in linear memory; cp_sp is a global.
```

## 3. WASM calling convention

```
//   exported                 imports
// -----------------------------------------------------------
// (module)
//   (func $pred_A_B (param i32 ptr_to_args) (result i32 succ?) … )
//   (memory (export "mem") 1)
//   (global $cp_sp (mut i32) (i32.const 0))
//   …
//
// JS wrapper keeps the pointer to args + cp_sp between calls.
```

*Call `pred/arity.next()`*: JS runtime passes current *cp* pop-ed from stack;
if none, it calls the predicate’s *entry block*.

The predicate returns **0** when it fails (no further answers) or **1** when it
has produced a value and left a choice-point (so JS should try again).

Value itself (the unified arguments) live in linear memory; the runtime knows
where the argument tuple begins because the wrapper allocated it.

## 4. Code-gen template

```
;; IR: (Gen *-> Block)
(loop $LOOP
  call $gen_func            ;; execute child generator once
  if (result i32)
    (then
      call $yield_current    ;; wrapper that snapshots cp & returns 1
      br $LOOP               ;; resume for more answers
    )
  end
)
```

`yield` alone simply becomes

```
  call $yield_current   ;; pushes resume-addr on CP stack
  return (i32.const 1)
```

where `$yield_current` helper:

1. stores env snapshot, current pc label into CP stack; increments `cp_sp`.
2. leaves the unified arguments untouched (they’re already in the arg tuple).

## 5. Example end-to-end

```
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
```

IR roughly becomes

```
defun(generator,$(member,2),
  allocate_vars([$(0)]),
  ( unify(CALLED_TERM, make_term(member,[X,List])) *->
      ( (List =.. [X|_], yield) ;
        ( List =.. [_|Tail], call member(X,Tail) ) )
  )
)
```

WASM (simplified WAT):

```
(func $member_2 (param $args i32) (result i32)
  (local $pc i32)        ;; current pc selector
  (local $env i32)       ;; addr of env frame

  ;; entry or via choice-point
  ;;    load $pc from CP      (JS put it there)  – 0 for entry
  block $END
  block $FAIL
  block $CASE1
  block $CASE2

  br_table $CASE1 $CASE2 $FAIL  ;; jump depending on $pc

  ;; CASE1 – List deconstruct, unify, success
  … unify logic …
  call $yield_current
  return

  ;; CASE2 – recurse
  … prepare new arg tuple …
  call $member_2
  return

  ;; FAIL – no answers left, clean CP stack
  i32.const 0
  return
)
```

## 6. Outstanding questions

1. **Garbage collection** of env snapshots – MVP uses bump-alloc; `cp_sp` reset
   by cut or at predicate exhaustion.
2. **Tail-recursion**: can we reuse the frame without growing linear memory?
3. How to surface *multiple* out-parameters cleanly to JS once unify moves into
   Wasm.

## 7. Implementation steps (incremental)

1. Provide `yield_current` & CP stack helpers in a tiny `.wat` runtime that the
   backend pre-pends.  Keep unify in JS for now.
2. Backend emits state-machine switch for each generator.
3. Teach compile_terms/3 to emit *two* functions per predicate:
   • `$pred_start` entry (calls resume helper)  
   • `$pred_cont` re-entry (receives pc).
4. JS wrapper hides all that behind the standard ES iterator
   (`return {next(){ … }}`).

---

This should provide a clear path: first minimal but correct behaviour under
Node, later rewrite unify/Term & GC for raw Wasm speed.
