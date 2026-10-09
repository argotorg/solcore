import * from std;
import * from std.dispatch;

// `new T[](n)` zero-initialises its elements, so the element's default value
// must be a valid value. A nested dynamic array element (T[][]) would leave
// every inner pointer null, and a later m[i] read/write would dereference it,
// an arbitrary memory access with no bounds protection. The compiler must
// reject this at the `new` site (see Parser/Expr.hs, newP).
contract C {
    function pwn(n : uint256) public returns (uint256) {
        let m : uint256[][] = new uint256[][](n);
        return m.length();
    }
}
