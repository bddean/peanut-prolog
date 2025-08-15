import {
  def_semidet,
  Var,
} from "pl-runtime";


def_semidet("var", 1, X => X instanceof Var);
def_semidet("nonvar", 1, X => !(X instanceof Var));
def_semidet("number", 1, X => ["number", "bigint"].includes(typeof X));
def_semidet("string", 1, X => typeof X === "string");
def_semidet("atom", 1, X => typeof X === "symbol");
