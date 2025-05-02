# Features
## TODO cut
i have an attempt in unstagd changes but i think i confused myself again.

see basis for a much simpler approach in ./simple-label-demo.js

1. gensym equivalent
2. Add Label:{_} Term
3. Pass containing label (instead of `Cont`) as context to goal_ir; this is not optional
	(cont is always yield)
4. then cut is (Label, (!, G), (G, break(Label))).
5. Profit!

We'll only generate the label jit and then unwrap on second compile pass.

----

wait a second break always does the inner thing. just wrap everything
in switch and it will work fine.

	NO -- we want OUTER break! I think! work out a solution...

~The solution here is probably to amend the IR to pass down the "context".~

Like, for loops: Add a label to the loop and generate !(loop(Label)) --> break Label;
... and for defun() -- !(function) --> return;

then for the runtime def we can't just do something like `_ ; G :-
call(G). G ; _ :- call(G).` because cuts within the first disjoined goal
needs to be "transparent", ie cut into ; itself. So instead do like:
`!, Gs ; R :- !, call(Gs).` Followed by the prev def... but not sure if
that actually works.

## TODO declarations
Not settled on the semantics for this but these would probably run at
a separate compile phase.

Make sure this is compatible with eventual self-hosting

## TODO Async predicates
Which would compile to async generators

## Other data structures
### TODO "compiled JS function" type
### TODO JS arrays
This could be a different prolog type... or just a special unification
type?
### TODO sets
I want a "set" type
## TODO Atoms as symbols(?)
## TODO modules
## TODO extensible unification
Codex, I have ideas about this not written here

# Testing infrastructure

We should work towards a single "quiet mode" command that runs all these
tests and only prints output for failures.

## TODO debugging integration tests
Put the generated IR and compiled JS fragment (not bundle) in the var/ directory
for debugging. Will need to add an export for terms_ir_debug to lib/comp.pl.

# Refactoring
## TODO Review code for general elegance / readability
## TODO Split lib/comp.pl into more subfiles.
Probably there should be some single source of truth defining the
IR type??

# Reliability / bugs
## Identifier symbols
fix internal js varnames (_1, args etc) so they can't collide with
predicate names

