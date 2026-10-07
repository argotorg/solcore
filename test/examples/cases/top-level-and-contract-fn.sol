import * from std;
import * from std.dispatch;

// A file may define a top-level function `f` and a contract that also has a
// method `f`: the two live in different scopes, so both definitions are
// accepted (regression for name clashes between the top-level term namespace
// and a contract's method namespace).

// Top-level free function f.
function f() returns (uint256) {
    return 42;
}

// Uses the top-level f (no contract in scope -> resolves to the free function).
function useTopLevelF() returns (uint256) {
    return f();
}

contract A {
    // A's own f, local to the contract's scope; coexists with the top-level f.
    function f() public returns (uint256) {
        return 7;
    }
}
