- comp.pl:
	- add a "walk" metapredicate for compiler passes over the IR
	- add passes that:
		1. convert *-> (for-each) to -> (if-then) if predicate property indicates det
		2. given a backend, walk tree to compile
	- add variable assignment for LVars
		- and atoms and terms...
	- see other TODO comments
- improve testing situation.
- update js backend to use new format
- improve variable code: allocate as vars at top of prediate. use\
  term_variables ->> numbervars for this
- special case impl for singleton ('_') vars? See numbervars options
- special case for lists.
- modules

- modifications to prolog types
	- dicts or even better, sets
	- we need a "compiled function" type...
		- and this needs to be serializable

- what about dynamic db? compiler is more fundamental... we can build
  inteprreted or jit mode on top
