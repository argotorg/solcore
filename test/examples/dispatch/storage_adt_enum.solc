import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

// An enumeration with more than two constructors.
//
// The SOP representation is a RIGHT-NESTED sum, so `Color` becomes
// `sum((), sum((), ()))` and the constructors are encoded as
//
//   Red   = inl(())            tag 0 at slot p
//   Green = inr(inl(()))       tag 1 at slot p, tag 0 at slot p+1
//   Blue  = inr(inr(()))       tag 1 at slot p, tag 1 at slot p+1
//
// so the tag is spread unary-style across the nesting levels and the type
// occupies 1 + max(0, 1 + max(0, 0)) = 2 slots.
//
// `Green` is the only constructor that exercises the `inr(inl(...))` path,
// which is exactly the sum nesting that CanStore.load has to reconstruct.
data Color = Red | Green | Blue;

// A three-constructor sum whose branches carry payloads of different widths.
// rep = sum(uint256, sum((uint256, uint256), ())), so
//   size = 1 + max(1, 1 + max(2, 0)) = 4.
data Shape =
      Dot(uint256)
    | Seg(uint256, uint256)
    | Nothing;

contract C {
    color : Color;
    shape : Shape;

    constructor() {
        color = Color.Red;
        shape = Shape.Nothing;
        // 1 tag + max(size (), 1 tag + max(size (), size ())) = 1 + 1 + 0 = 2
        assert(StorageSize.size(Proxy : Proxy(Color)) == 2);
        // 1 tag + max(size uint256, 1 tag + max(size (uint256,uint256), size ())) = 1 + 1 + 2 = 4
        assert(StorageSize.size(Proxy : Proxy(Shape)) == 4);
    }

    public function setRed() -> () {
        color = Color.Red;
    }

    // inr(inl(())) — the nested-tag branch.
    public function setGreen() -> () {
        color = Color.Green;
    }

    public function setBlue() -> () {
        color = Color.Blue;
    }

    public function tag() -> uint256 {
        match color {
        | Color.Red   => return uint256(0);
        | Color.Green => return uint256(1);
        | Color.Blue  => return uint256(2);
        }
    }

    public function setDot(a : uint256) -> () {
        shape = Shape.Dot(a);
    }

    // inr(inl(...)) again, this time with a product payload.
    public function setSeg(a : uint256, b : uint256) -> () {
        shape = Shape.Seg(a, b);
    }

    public function setNothing() -> () {
        shape = Shape.Nothing;
    }

    public function shapeSum() -> uint256 {
        match shape {
        | Shape.Dot(a)    => return a;
        | Shape.Seg(a, b) => return a + b;
        | Shape.Nothing   => return uint256(0);
        }
    }

    public function shapeTag() -> uint256 {
        match shape {
        | Shape.Dot(_)    => return uint256(0);
        | Shape.Seg(_, _) => return uint256(1);
        | Shape.Nothing   => return uint256(2);
        }
    }
}
