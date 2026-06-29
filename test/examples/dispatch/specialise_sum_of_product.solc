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

import std.{*};
import std.dispatch.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

// total(x, y) sums every leaf word of both arguments.
forall a.
class a : Total {
  function total(x : a, y : a) -> word;
}

instance word : Total {
  function total(x : word, y : word) -> word {
    return x + y;
  }
}

// product: recurse into both components (this is the shape inl carries).
forall f g . f : Total, g : Total => instance (f, g) : Total {
  function total(x : (f, g), y : (f, g)) -> word {
    match x {
    | (xa, xb) => match y {
                  | (ya, yb) => return Total.total(xa, ya) + Total.total(xb, yb);
                  }
    }
  }
}

// sum: the buggy shape. The inl branch recurses at f (a product here), the inr
// branch recurses at g (a word here); specializing one must not pollute the
// other's nested `match y`.
forall f g . f : Total, g : Total => instance sum(f, g) : Total {
  function total(x : sum(f, g), y : sum(f, g)) -> word {
    match x {
    | inl(xa) => match y {
                 | inl(ya) => return Total.total(xa, ya);
                 | inr(yb) => return 0;
                 }
    | inr(xb) => match y {
                 | inl(ya) => return 0;
                 | inr(yb) => return Total.total(xb, yb);
                 }
    }
  }
}

contract SpecialiseSumOfProduct {
    constructor() {}

    // inl carries a product (word, word); the two sum sides differ in shape
    // (pair vs word), which is what the specializer mishandled.
    // total(inl((1,2)), inl((1,2))) = total((1,2),(1,2)) = (1+1)+(2+2) = 6.
    public function probe() -> uint256 {
        let x : sum((word, word), word) = inl((1, 2));
        let y : sum((word, word), word) = inl((1, 2));
        return uint256(Total.total(x, y));
    }
}
