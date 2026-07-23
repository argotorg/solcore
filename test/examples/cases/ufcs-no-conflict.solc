import std.{*};

// Regression test: the UFCS (receiver-style) method-call rewriting in
// NameResolution must coexist with the other uses of dot syntax without
// hijacking any of them. All of the following appear in one contract:
//
//   * UFCS call            val.combine(z)      ==> Combiner.combine(val, z)
//   * qualified class call Combiner.combine(val, z)   (must stay as-is)
//   * qualified constructor Color.Red                 (dotted constructor)
//   * plain field read      val
//
// UFCS only fires when the receiver is an (unqualified) contract field, so a
// receiver that resolves to a class/module name (`Combiner.combine(...)`) or a
// type name (`Color.Red`) is handled by the earlier qualified-name cases and
// never reaches the UFCS rule.

forall a.
class a : Combiner {
  function combine(x : a, y : word) -> word;
}

instance word : Combiner {
  function combine(x : word, y : word) -> word {
    return y;
  }
}

data Color = Red | Green;

contract UfcsNoConflict {
  val : word;

  constructor() {}

  // UFCS receiver call on a contract field.
  public function viaUfcs(z : word) -> word {
    return val.combine(z);
  }

  // The explicit qualified class call for the same method: NOT rewritten by
  // UFCS (receiver is the class name `Combiner`, not a field).
  public function viaQualified(z : word) -> word {
    return Combiner.combine(val, z);
  }

  // A dotted constructor and a bare field read still resolve normally
  // alongside the UFCS rule.
  public function dottedConstructorAndFieldRead() -> word {
    let c : Color = Color.Red;
    return val;
  }
}
