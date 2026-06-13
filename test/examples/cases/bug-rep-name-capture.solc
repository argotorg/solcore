// Bug: local variable named `rep` causes name capture with the type variable `rep`
// from `class abs : Typedef(rep)`.  In NameResolution.hs, the S.ExpVar and
// S.ExpName cases used a wildcard `_` for the qualifier in patterns like
// `(_, Just TLocalVar)`, so a qualified call `Typedef.rep(a)` resolved to the
// local variable `rep` instead of the class method.
//
// Expected: compiles successfully; `Typedef.rep` resolves to the class method.
// Actual (before fix): PANIC: no resolution found for invokable.invoke

import std.{*};
import std.dispatch.{*};
pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

contract Bug {
    constructor() {}

    function f(a : uint256) -> uint256 {
        let rep : uint256 = a;
        let w : word = Typedef.rep(a);
        return Typedef.abs(w);
    }
}
