def_js_predicate(var/1, "V => V instanceof Var").
def_js_predicate(term/1, "T => T instanceof Term").
def_js_predicate('==='/2, "(A, B) => A === B").

def_js_generator(bindvar/2, "function*(V, X) {
  if(!(V instanceof Var)) return
	v.ref = X;
	yield;
	v.ref = UnboundSym;
}")

def_js_generator(functor/3, "function*(T, N, A) {
  if(T instanceof Var) {
		if (N instanceof Var || A instanceof Var) {
			throw new Error('insufficiently instantiated');
		}
		T.ref = new Term(N, A);
		yield;
		T.ref = UnboundVar;
	} else if (T instanceof Term) {
		if (T
	}
}")





def_js_expr(!/0, "break;");


var(A) :- apply_js_predicate("

", [A]).

%%%%

A = A.
