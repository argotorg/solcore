// Regression test: specializer sum-of-product bug (specMatch substitution leak).
//
// A binary class method over the primitive `sum(f, g)` whose two sides have
// DIFFERENT shapes: the inl side carries a product (word, word), the inr side
// carries a plain word. Specializing the instance at sum((word, word), word)
// used to leak a substitution binding from one match alternative into the
// sibling alternative's nested `match`, mistyping its scrutinee. The frontend
// (sol-core) accepted the program, but `yule` then rejected the emitted .hull:
//
//   Type mismatch
//     expected: sum(word, word)
//     actual:   sum(pair(word, word), word)
//
// Root cause: in Specialise.hs, `specMatch` did not scope `spSubst` (a global
// accumulator) across match alternatives. While specializing the `inl` branch,
// a binding leaked into the `inr` branch's nested `match`, collapsing
// sum(f, g) to sum(g, g). The fix resets spSubst around each alternative.
//
// This isolates the SPECIALIZER: no #[derive], no Eq universe instances. The
// class and its instances are defined locally and exercised directly, so the
// program must now lower end-to-end and return the expected value.

import {*} from std;
import {*} from std.dispatch;

pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;

// total(x, y) sums every leaf word of both arguments.
trait Total<a> {
  function total(x : a, y : a) returns (word);
}

impl Total<word> {
  function total(x : word, y : word) returns (word) {
    return x + y;
  }
}

// product: recurse into both components (this is the shape inl carries).
impl<f, g> Total<(f, g)> where f: Total, g: Total {
  function total(x : (f, g), y : (f, g)) returns (word) {
    match (x ) {
    case (xa, xb) { match (y ) {
                  case (ya, yb) { return Total.total(xa, ya) + Total.total(xb, yb);
                  } }
    } }
  }
}

// sum: the buggy shape. The inl branch recurses at f (a product here), the inr
// branch recurses at g (a word here); specializing one must not pollute the
// other's nested `match y`.
impl<f, g> Total<sum<f, g>> where f: Total, g: Total {
  function total(x : sum<f, g>, y : sum<f, g>) returns (word) {
    match (x ) {
    case inl(xa) { match (y ) {
                 case inl(ya) { return Total.total(xa, ya);
                 } case inr(yb) { return 0;
                 } }
    } case inr(xb) { match (y ) {
                 case inl(ya) { return 0;
                 } case inr(yb) { return Total.total(xb, yb);
                 } }
    } }
  }
}

contract SpecialiseSumOfProduct {
    constructor() {}

    // inl carries a product (word, word); the two sum sides differ in shape
    // (pair vs word), which is what the specializer mishandled.
    // total(inl((1,2)), inl((1,2))) = total((1,2),(1,2)) = (1+1)+(2+2) = 6.
    function probe() public returns (uint256) {
        let x : sum<(word, word), word> = inl((1, 2));
        let y : sum<(word, word), word> = inl((1, 2));
        return uint256(Total.total(x, y));
    }
}
