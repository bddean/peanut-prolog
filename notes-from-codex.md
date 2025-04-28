## Project snapshot

    This repo is an experimental “tiny‑Prolog compiler”.
    The idea is to take a normal set of Prolog clauses, translate them
    into a small,
    language‑neutral IR, and then spit out runnable source code for a
    chosen
    backend (JavaScript today, Go is sketched).  A miniature runtime that
    implements variables, unification and back‑tracking is shipped with
    the
    generated code.

    ## Directory map

    • lib/
      • prelude.pl – a handful of hand‑written predicates (member/2,
    append/3 …) that are always available.
      • comp.pl  – the front‑end / IR builder.
        – compile_clauses/2   : takes a list of clauses that belong to one
     predicate and produces an IR tree.
        – compile_goal/2      : lowers individual goals inside a clause.
        – Tests at the bottom (:- begin_tests(comp).).
      • backend/
        • js.pl       – main JavaScript code‑gen DCG.  Walks the IR and
    emits JS
          (using generators to model Prolog choice points).
    • compile.pl  – command‑line driver: loads a .pl file, calls comp.pl → backend/js.pl, writes output.js.
    • runtime.ts – tiny JS runtime that the generated code relies on.
    Defines
      Var, Term, unify, call/1, etc.; uses ES6 generators for
      back‑tracking.
    • TODO.md     – work list for the next compiler milestones (see
    below).

    ## The intermediate representation

    A few examples (all ordinary Prolog input):

        1. Fact   `foo(1).`   ⇒   `funcall("unify", ["X", \foo(1)]) *->
    yield`
        2. Rule   `foo(X):-bar(X),baz.`

           is turned into

               funcall("unify",["X",\foo('$VAR'(0))]) *->
                   (funcall(bar,[\'$VAR\'(0)]) *->
                    funcall(baz,[]) *-> yield)
        3. A whole predicate becomes

               defun(generator, Name/Arity, "X", ImplTree)

    The back‑end chooses how to render:

    • js.pl emits something like

        function* foo_1(X) {
          const [_1] = Var.allocate();
          for (const _ of unify(X, foo(1))) {
            yield;
          }
          for(const _ of unify(X, foo(_1))) {
            for(const _ of bar(_1)) {
              yield* baz();
            }
          }
        }

    ## State of the code

    The compiler is able to:

    ✓ Parse / normalise clauses (variable numbering, conjunction,
    disjunction, cut).
    ✓ Produce a working IR.
    ✓ Convert that IR to runnable JavaScript whose behaviour matches the
    source
      predicate, using the runtime in runtime.ts.
    ✓ Pass all PLUnit tests in lib/comp.pl

    ## Mental model to keep while hacking

    Front‑end (Prolog) → IR    (comp.pl)
    IR → Target Code           (backends/js.pl, future backends)
    Generated Code + runtime.ts = Stand‑alone executable program.

    If you want to play:

        $ swipl -q -f compile.pl -- test/in//empty.pl sample.js
        $ node sample.js

    That’s the gist of the repo as of now.
