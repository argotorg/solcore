import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

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
enum Color { Red, Green, Blue }

// A three-constructor sum whose branches carry payloads of different widths.
// rep = sum(uint256, sum((uint256, uint256), ())), so
//   size = 1 + max(1, 1 + max(2, 0)) = 4.
enum Shape { Dot(uint256), Seg(uint256, uint256), Nothing }

contract C {
    color : Color;
    shape : Shape;

    constructor() {
        color = Color.Red;
        shape = Shape.Nothing;
        // 1 tag + max(size (), 1 tag + max(size (), size ())) = 1 + 1 + 0 = 2
        assert(StorageSize.size(Proxy as Proxy<Color>) == 2);
        // 1 tag + max(size uint256, 1 tag + max(size (uint256,uint256), size ())) = 1 + 1 + 2 = 4
        assert(StorageSize.size(Proxy as Proxy<Shape>) == 4);
    }

    function setRed() public returns (()) {
        color = Color.Red;
    }

    // inr(inl(())) — the nested-tag branch.
    function setGreen() public returns (()) {
        color = Color.Green;
    }

    function setBlue() public returns (()) {
        color = Color.Blue;
    }

    function tag() public returns (uint256) {
        match (color ) {
        case Color.Red   { return uint256(0);
        } case Color.Green { return uint256(1);
        } case Color.Blue  { return uint256(2);
        } }
    }

    function setDot(a : uint256) public returns (()) {
        shape = Shape.Dot(a);
    }

    // inr(inl(...)) again, this time with a product payload.
    function setSeg(a : uint256, b : uint256) public returns (()) {
        shape = Shape.Seg(a, b);
    }

    function setNothing() public returns (()) {
        shape = Shape.Nothing;
    }

    function shapeSum() public returns (uint256) {
        match (shape ) {
        case Shape.Dot(a)    { return a;
        } case Shape.Seg(a, b) { return a + b;
        } case Shape.Nothing   { return uint256(0);
        } }
    }

    function shapeTag() public returns (uint256) {
        match (shape ) {
        case Shape.Dot(_)    { return uint256(0);
        } case Shape.Seg(_, _) { return uint256(1);
        } case Shape.Nothing   { return uint256(2);
        } }
    }
}
