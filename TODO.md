NEXT
fix ident <--> string conversion


[ ] test imrpvoements
	[ ] unit tests -> separate files
	[ ] set up good integration test setup
		* artifacts for debugging: IR, logs (?), trace (?),
		* benchmarks
	[ ] fill gaps in unit tests

[ ] claude.md

[ ] refactoring
	[ ] rm ir_to_js; also have compile preds accept DCGs for concat
	[ ] more files
	[ ] move walk_ir to an ir.pl. And maybe make it more general / easy to extend...

[ ] TODO comments

[ ] support declarations
[ ] modules
[ ] fix atom and term representation...

- comp.pl:
	- add passes that:
		1. convert *-> (for-each) to -> (if-then) if predicate property indicates det

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
