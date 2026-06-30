import std.{*};
import std.dispatch.{*};

// Regression test for a yule backend bug, independent of the storage/Generic
// work: matching a sum constructor whose payload is a product of arity >= 3.
//
// On `match`, the scrutinee's location is flattened, and the constructor payload
// used to be bound as a flat slot sequence. Destructuring the inner product then
// did EFst on a >2-element sequence and crashed yule with "EFst: type mismatch".
// (A 2-field payload happened to work, since a flat 2-seq is a valid pair.)
//
// No storage and no Generic derivation involved — just constructing and matching
// an ordinary algebraic data type.

data Shape = Dot | Tri(uint256, uint256, uint256);

contract C {
    constructor() {}

    // Build Tri(a,b,c) then match it back out: exercises a sum whose payload is
    // a 3-field product.
    public function triSum(a : uint256, b : uint256, c : uint256) -> uint256 {
        let s : Shape = Shape.Tri(a, b, c);
        match s {
        | Shape.Dot          => return uint256(0);
        | Shape.Tri(x, y, z) => return x + y + z;
        }
    }
}
