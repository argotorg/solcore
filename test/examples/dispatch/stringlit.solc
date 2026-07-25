import {*} from std;
import {memory, string, uint256} from std;
import {*} from std.dispatch;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

// End-to-end test of comptime string materialization into memory(string):
// each form must ABI-encode to the same "Hello, world!" return value.
contract C {
    constructor() {}

    // terse: concatLit wrapped in Str.fromString by the desugarer
    function greeting() public returns (string memory) {
        return concatLit("Hello, ", "world!");
	// fromString inserted automatically when using concatLit
	// later we may have an operator for that e.g. <>
    }

    // A2: via an intermediate string-typed let (dead-let substitution path)
    function greetLet() public returns (string memory) {
        let s : string = "Hello, " + "world!";
        return Str.fromString(s);
	// here fromString needs to be inserted manually
    }
}
