import { Var, makeTerm, registerModule, db_set, db_get } from 'pl-runtime';
import "pl-library/prelude";
const $_gen1=function* (...args) { 
const $_CALLED_TERM=Symbol.for("main");
;
const [$_0, $_1, $_2, $_3] = Var.allocate();
;
for (const _ of db_get("user:unify/2")($_CALLED_TERM, Symbol.for("main"))) {
for (const _ of db_get("user:=/2")($_0, makeTerm(Symbol.for("[|]"), [Symbol.for("a"), makeTerm(Symbol.for("[|]"), [Symbol.for("b"), makeTerm(Symbol.for("[|]"), [Symbol.for("c"), Symbol.for("[|]")])])]))) {
for (const _ of db_get("user:maplist/3")(Symbol.for("="), $_0, $_1)) {
for (const _ of db_get("user:array_list/2")($_2, $_1)) {
for (const _ of db_get("user:writeln/1")(Symbol.for("to_array"))) {
yield* db_get("user:writeln/1")($_2);

}
}
}
}
};

};
;
db_set("user:main/0", $_gen1);
direct:{
for (const _ of db_get("user:main/0")()) {
break direct;

}
}