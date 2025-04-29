# Testing infrastructure

We should work towards a single "quiet mode" command that runs all these
tests and only prints output for failures.

## TODO move plunit tests to separate files
## TODO update unit tests
Fill gaps in suite, and fix failures

Some of the tests may just be outdated / for an older interface.
## TODO debugging integration tests
Put the generated IR and compiled JS fragment (not bundle) in the var/ directory
for debugging. Will need to add an export for terms_ir_debug to lib/comp.pl.

# Refactoring
## TODO Review code for general elegance / readability
## TODO Split lib/comp.pl into more subfiles.
Probably there should be some single source of truth defining the
IR type??
## TODO Remove the ir_to_js export from the js backend
instead rely on js/3 and amend comp.pl to use codes (dif lists)

# Reliability / bugs
## Identifier symbols
fix internal js varnames (_1, args etc) so they can't collide with
predicate names

# Features
## TODO cut
The solution here is probably to amend the IR to pass down the "context".

Like, for loops: Add a label to the loop and generate !(loop(Label)) --> break Label;
... and for defun() -- !(function) --> return;

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

