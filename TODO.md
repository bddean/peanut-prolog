# Features
## Other control structures
https://www.swi-prolog.org/pldoc/man?predicate=!/0
## call/n
* ehhh just implement as intrinsic for now?? then we can use eval
* or at least, like, call_single_term...

## TODO directives
Not settled on the semantics for this but these would probably just run
at compile time. similar to Ciao Prolog.

Make sure this is compatible with eventual self-hosting

## TODO Async predicates
Which would compile to async generators

## Other data structures
### TODO "compiled JS function" type
### TODO JS arrays
This could be a different prolog type... or just a special unification
type?
### TODO sets
I want a "set" type like SWI's dicts but more general

https://platform.openai.com/playground/prompts?preset=aNjqRnCeYPbMtoL6sR3HMQe5
## TODO Atoms as symbols(?)
## TODO modules
## TODO extensible unification
## TODO types
Like Ciao's assertion language
Impl: Theorem prover? CHR type deal?

# Testing infrastructure

## TODO quiet mode for unit tests (only log failures)

## TODO debug artifacts for integration tests
Put the generated IR and compiled JS fragment (not bundle) in the var/ directory
for debugging. Will need to add an export for terms_ir_debug to lib/comp.pl.


# Refactoring
## TODO Review code for general elegance / readability
## TODO Split lib/comp.pl into more subfiles.
Probably there should be some single source of truth defining the
IR type??
