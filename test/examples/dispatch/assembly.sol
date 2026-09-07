import {*} from std;
import {*} from std.dispatch;

contract C {
    constructor() {}

    // Exercises a Yul block that declares an uninitialized `let y`, assigns the
    // boolean literal `true` to it, and writes it back to the surrounding
    // `word` local `x`. `true` is the word `1`, so this returns uint256(1).
    function asmBool() public returns (uint256) {
        let x : word;
        assembly {
          let y
          y := true
          x := y
        }
        return uint256(x);
    }
}
