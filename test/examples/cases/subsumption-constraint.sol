// This code should FAIL, but PASSES!
enum Bool { True, False }

trait MyCls<a> {
  function f(x : a, y : a) returns (Bool);
}

function the_bug<a>(x : a, y : a) returns (Bool) {
    return MyCls.f(x, y);
}

contract Foo {
    function x() public {
        let b1 = Bool.True;
        let b2 = Bool.False;
        the_bug(b1, b2);
    }

    function main() public {
        x();
    }
}
