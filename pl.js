"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.loadModule = exports.run_goal = exports.add_fact = exports.Term = exports.Atom = exports.Var = void 0;
exports.call = call;
const UnboundSym = Symbol("Unbound Variable");
let _vid = 0; // For debugging etc.
class Var {
    constructor() {
        this.ref = UnboundSym;
        this.id = _vid++;
    }
    ;
    *set(v) {
        if (this.ref !== UnboundSym)
            return; // TODO skip this check for unify??
        this.ref = v;
        yield;
        this.ref = UnboundSym;
    }
}
exports.Var = Var;
let CURRENT_MODULE = "user";
const Atom = (name) => Symbol.for(CURRENT_MODULE + ":" + name);
exports.Atom = Atom;
class Term {
    constructor(tag, args) {
        this.tag = tag;
        this.args = args;
    }
    copy(vars = new Map()) {
        return new Term(this.tag, this.args.map(A => {
            const X = deref(A);
            if (X instanceof Term)
                return X.copy(vars);
            if (!(X instanceof Var))
                return X;
            if (!vars.has(X))
                vars.set(X, new Var());
            return vars.get(X);
        }));
    }
}
exports.Term = Term;
function deref(v) {
    if (!(v instanceof Var))
        return v;
    if (v.ref === UnboundSym)
        return v;
    return deref(v.ref);
}
function* unify(A, B) {
    A = deref(A);
    B = deref(B);
    if (A === B)
        return yield;
    if (A instanceof Var)
        return yield* A.set(B);
    else if (B instanceof Var)
        return yield* B.set(A);
    else if (A instanceof Term && B instanceof Term) {
        if (A.tag !== B.tag
            || A.args.length !== B.args.length)
            return;
        return yield* unifyArgs(A.args, B.args);
    }
    // Else, fail.
}
function* unifyArgs(A, B, i = 0) {
    if (i >= A.length)
        return yield;
    for (const _ of unify(A[i], B[i])) {
        yield* unifyArgs(A, B, i + 1);
    }
}
const db = new Map();
function* call(goal) {
    var _a;
    goal = deref(goal);
    if (goal instanceof Var)
        throw new Error("Can't call var.");
    const [tag, arity] = goal instanceof Term
        ? [goal.tag, goal.args.length]
        : [goal, 0];
    const entries = (_a = db.get(tag)) === null || _a === void 0 ? void 0 : _a.get(arity);
    if (!entries) {
        throw new Error("Goal not found: " + goal);
    }
    // if (!entries) return;
    for (let entry of entries) {
        if (typeof entry === "function") {
            yield* entry(...(goal instanceof Term ? goal.args : []));
        }
        else if (entry instanceof Term) {
            entry = entry.copy();
            if (entry.tag === ":-" && entry.args.length === 2) {
                const [head, body] = entry.args;
                for (const _ of unify(goal, head)) {
                    yield* call(body);
                }
            }
            else
                yield* unify(goal, entry);
        } // Else fail.
    }
}
// nosubmit asserta?
function* assertz(v) { }
const add_fact = (tag, arity, Fact) => {
    if (!db.has(tag))
        db.set(tag, new Map());
    const byArity = db.get(tag);
    if (!byArity.has(arity))
        byArity.set(arity, []);
    byArity.get(arity).push(Fact);
};
exports.add_fact = add_fact;
const run_goal = (G) => call(G).next();
exports.run_goal = run_goal;
const loadCtx = { add_fact: exports.add_fact, run_goal: exports.run_goal };
// nosubmit hmm maybe move towards more of 1:1 mapping to js modules
// - atoms: just symbols these are the ONLY exports.
// - use_moduel --> import
// ... but we need to make sure it's serializable so use Symbol.for(mod:atom) etc
const loadModule = (name, loader) => {
    CURRENT_MODULE = "name";
    try {
        loader(loadCtx);
    }
    finally {
        CURRENT_MODULE = "user";
    }
};
exports.loadModule = loadModule;
db.set("writeln", new Map([[1, [function* (arg) {
                console.log(arg);
                yield;
            }]]]));
db.set(",", new Map([[2, [function* (g1, g2) {
                for (const _ of call(g1))
                    yield* call(g2);
            }]]]));
db.set("fail", new Map([[0, [function* () { }]]]));
// TODO call/n... though that can also just be def'd in prolog...
db.set("call", new Map([1].map(n => [n, [call]])));
