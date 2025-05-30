## Next Major Steps

1. **Module resolution during compilation**: Change compiler to load Prolog files into runtime first, let term_expansion work, then compile the result. This will properly resolve module:predicate/arity forms at compile-time instead of guessing at runtime.

2. **Fix built-in predicate resolution**: Currently calls from within modules try to resolve all predicates to that module (e.g. hello:writeln/1). Need proper fallback to user: for built-ins.

3. **Self-hosting**: Once module resolution works properly, enable self-hosted compilation.

## Recently Completed

✅ Database-based module system (db_set/db_get) 
✅ Removed function identifier complexity
✅ Simplified import/export (no predicate-level tracking)
✅ Module-aware compilation with proper key format

~~~
NEXT rework

- use esm modules for importing but a global db for exporting.
	- the compiler will extract db lookups for *known*, *static*
		predicates to variable lookups
		- (so we still need module-closure terms like compiled_host_fn/3 below)
- Self-hosted compiler. Needs to dynamically load module before compiling
	it so term_expansion, directives etc work.
	- dynamically loading works: read a term at a time, asserting to DB.
		- and then add "dynamic module" to mod registry
	- multifile predicates from other moduels need an api to dynamically epxand
		- (multifile & dynamic likely the same under the hood, actually)
- Module qualification should be done purely with expand term. Compiler
	not involved. (Reading depended-on pl modules unavoidable to support
	expand_term at all).

------------------------------------------------------------------------

So the compiler should be more like: Load program dynamically, and then
compile it.

In particular. Compile + eval a predicate at a time. And then, separately, read the source and compile it.
(Or I guess other way around. E.g. term_expansion shouldn't apply to self...)

requirements:
- assertz/1 but not clause/2.
- a new "dynamic module" type for the runtime. Necessary since we can't add to NS of esm modules.

# Features
## TODO module fixups
- fix special modules like runtime / system / user
- fix paths

## TODO term and clause expansion
## IN PROG meta predicate namespacing
## TODO Other control structures
## TODO dynamic predicates
https://www.swi-prolog.org/pldoc/man?predicate=!/0
## TODO other basic builtins
## DONE (for now) - call/n
- see ./notes/call-ideas.md
-  todo-- module closure terms which unify to fns
		compiled_host_fn(js, mymod, "function*() {
		  console.log('test', importedsymbol);
		  yield;
		}")

## IN PROGRESS directives
Not settled on the semantics for this but these would probably just run
at compile time. similar to Ciao Prolog.

Make sure this is compatible with eventual self-hosting

For now, just hardcode some. This is similar to what Ciao does.

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
## TODO Optimize atoms
Hoist Symbol.for calls. (Probably add create_symbol to ir).
## DONE modules (for now)
import all is buggy but maybe i can fix after self hosting worjs

## TODO extensible unification
## TODO types
Like Ciao's assertion language
Impl: Theorem prover? CHR type deal?

# Testing infrastructure

## TODO Benchmarks!!!

## TODO quiet mode for unit tests (only log failures)

## TODO debug artifacts for integration tests
Put the generated IR and compiled JS fragment (not bundle) in the var/ directory
for debugging. Will need to add an export for terms_ir_debug to src/comp.pl.


# TODO Improve ISO compat
I can keep string/atom/chars distinction by using backticks for strings

This is recommended in some post critiquing SWI 7's compat breaking
changes. SWI supports this syntax with flags
