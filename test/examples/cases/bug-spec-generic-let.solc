// Bug: specStmt (Let i mty (Just e)) always called `atCurrentSubst i` AFTER
// `specExp`, causing `extSpSubst phi` (with original type-variable names) to
// corrupt subsequent let bindings.  Concretely, `b_decoded : (uint256,uint256)`
// was mangled to `uint256` inside the ABIDecode instance for pairs.
//
// Root cause: when `ty'` is already concrete (freetv ty' == []), re-applying
// `atCurrentSubst` after `specExp` risks picking up unrelated bindings added
// by nested `specCall` invocations (e.g. {b -> uint256} from an inner decode).
//
// Fix: only re-apply when `freetv ty'` is non-empty (open type that needs
// resolution by the RHS, as in `let r : rep = Generic.from(x)`).
//
// Expected: compiles successfully.
// Actual (before fix): PANIC: Type mismatch expected uint256 actual (uint256,uint256)

import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

data Pair = MkPair(uint256, uint256);

instance Pair : Generic((uint256, uint256)) {
    function from(x : Pair) -> (uint256, uint256) {
        match x { | Pair.MkPair(a, b) => return (a, b); }
    }
    function to(x : (uint256, uint256)) -> Pair {
        match x { | (a, b) => return Pair.MkPair(a, b); }
    }
}

contract BugSpecGenericLet {
    constructor() {}

    function roundtrip(a : uint256, b : uint256) -> uint256 {
        let p : Pair = Pair.MkPair(a, b);
        let encoded : memory(bytes) = abi_encode(p);
        let decoded : (uint256, uint256) = abi_decode(encoded, @(uint256, uint256), @MemoryWordReader);
        match decoded {
        | (x, y) =>
            match and(Eq.eq(x, a), Eq.eq(y, b)) {
            | true  => return uint256(1);
            | false => return uint256(0);
            }
        }
    }
}
