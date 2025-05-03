# Project

This is a Prolog-to-JS compiler written in SWI-Prolog. Very early stages.

Source is in ./lib. A JS runtime is compiled from ./runtime.ts.

TODO list in in ./TODO.md. Mark items as IN_PROGRESS and DONE as you go
through the list. Feel free to add tasks.


# Style

Commit very often. Commit messages MUST with 'CODEX:'.

You MUST prioritize elegance and completeness. Existing code may not be
exemplary :-)

Very incremental TDD. Use simple inline plunit tests to verify
implementation details as you go, own-file unit tests for more complex
things, but especially integration tests that run end-to-end.

# Useful commands / patterns

swipl -g 'apropos(...)' -g halt

swipl -g 'help(...)' -g halt

swipl -l ... -g 'run_tests' -g halt

swipl -l ... -g 'run_tests(Spec)' -g halt

# Future Project goals

* JS integration and web-dev utility 
* Efficient
* Self-hosted


-------------------------------------------------------------------------------
Repository-at-a-glance
-------------------------------------------------------------------------------

Top-level layout (paths are relative to the repo root):

• lib/               Front-end & back-ends, all written in Prolog
  • comp.pl          – reads *.pl files → builds IR (intermediate representation)
  • prelude.pl       – built-ins that are always available to user code
  • optimizations.pl – IR rewrites (peephole etc.)
  • backend/
    • js.pl          – IR → JavaScript generator
    • js_identifier.pl – helper for valid JS identifiers

• runtime.ts         Tiny JS runtime (Var, Term, unify/2 …) that the generated
                     code relies on; compiled to runtime.js which is bundled
                     with every program.

• compile.pl         Command-line driver: loads comp.pl + backend/js.pl, turns a
                     Prolog source file into JS.

• devbin/            Convenience wrappers (exposed on $PATH via .envrc)
  • @bundle          – compile <file.pl>, spit runnable JS to stdout
  • @test-unit       – run plunit suites       (via swipl)
  • @test-intg       – run integration tests  (generate JS → run node → diff)
  • @test            – both of the above.

• test/              Test input and harness
  • in/              Sample *.pl programs used by the integration suite
  • var/             Scratch dir for any artefacts the tests wish to keep

-------------------------------------------------------------------------------
Quick start
-------------------------------------------------------------------------------

All commands assume you are in the repo root **and** you have either

    $ direnv allow    # or exported PATH + PLTS_HOME manually (see below)

so that the helper scripts are on your PATH.

1. Compile a source file and run it with Node:

       $ @bundle test/in/hello.pl | node

   (The helper concatenates runtime.js, the generated code and a tiny driver.)

2. Run unit tests (plunit blocks inside Prolog source files):

       $ @test-unit            # all
       $ @test-unit comp       # just lib/comp.pl:* tests

3. Run integration tests:

       $ @test-intg

4. Everything at once:

       $ @test

-------------------------------------------------------------------------------
Build / runtime environment
-------------------------------------------------------------------------------

Required versions (rough guidelines – exact patch version rarely matters):

• SWI-Prolog ≥ 9.0   (modules, gensym/2, plunit)
• Node ≥ 18          (ES2019+, generators)
• TypeScript ≥ 5.0   (to rebuild runtime.js from runtime.ts)

No external SWI packs are needed.

Environment variables
---------------------

• PATH is extended by .envrc so that every helper inside devbin/ can be
  invoked as “@…”.
• PLTS_HOME is automatically exported to the repo root.  `devbin/__run` uses
  this to locate the project even when called from sub-directories.

If you don’t use direnv you can set these manually:

    $ export PATH="$PWD/devbin:$PATH"
    $ export PLTS_HOME="$PWD"

-------------------------------------------------------------------------------
Commit & review workflow
-------------------------------------------------------------------------------

• Commit early & often.  Every message must start with "CODEX:".
• Push only green tests.  If you create new failing tests, mark them with
  `blocked/1` so CI stays green.

-------------------------------------------------------------------------------
Testing philosophy
-------------------------------------------------------------------------------

1. Inline plunit for small, self-contained helper predicates.
2. Integration tests to catch “compile → execute → same observable behaviour”
   regressions.  They live in test/in/*.pl.  The harness compares stdout and
   stderr of original SWI vs. compiled JS.

Tip: to debug a single integration sample, run it through @bundle and inspect
the generated JS in any editor.

-------------------------------------------------------------------------------
Compiler pipeline (mental model)
-------------------------------------------------------------------------------

   Prolog source (.pl)
        │  front-end            comp.pl   →  IR (a small Prolog term)
        │  back-end             backend/js.pl →  JavaScript generator functions
        ▼                          ▼
   Generated JS  +  runtime.js  ───►  node  ───► behaviour identical to source

The IR is documented inline in lib/comp.pl.  A predicate foo/2 becomes

    defun(generator, foo/2, BodyIR).

Within BodyIR the control structures make back-tracking explicit (yield*, *->, …).

-------------------------------------------------------------------------------
Troubleshooting snippets
-------------------------------------------------------------------------------

• “undefined predicate js/3”:  make sure you loaded lib/backend/js.pl before
  calling comp:compile_terms/3.
• Runtime error in generated code?  Use   `node --trace-uncaught …`  to get a
  stack trace, then map the line back to IR with @bundle ‑-debug (TBD).

