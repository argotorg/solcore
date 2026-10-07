import * from std;
import * from std.dispatch;

// Two contracts in one file may each declare a method of the same name: each
// `f` is local to its own contract, so this must compile (regression for a
// spurious cross-contract SC0225 "duplicate function definition").

contract A {
    function f() public {
    }
}

contract B {
    function f() public {
    }
}
