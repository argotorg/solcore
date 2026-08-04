import std.{*};
import std.dispatch.{*};

// End-to-end (runs on evmone via the testrunner) check that Yul `break`,
// `continue` and `leave` in inline assembly don't just parse, but actually
// execute with the right control-flow semantics.
contract C {
    constructor() {}

    // `continue` + `break`: sum i over [0, 10), skipping i < 2 (continue) and
    // stopping once i > 5 (break). So only i in {2,3,4,5} contribute:
    //   2 + 3 + 4 + 5 = 14
    // A miscompiled `continue` would also add 0 and 1 (=> 15); a broken `break`
    // would keep going and add 6..9 as well.
    public function loopSum() -> uint256 {
        let result : word;
        assembly {
            result := 0
            for { let i := 0 } lt(i, 10) { i := add(i, 1) } {
                if lt(i, 2) {
                    continue
                }
                if gt(i, 5) {
                    break
                }
                result := add(result, i)
            }
        }
        return uint256(result);
    }

    // `leave`: clamp(x) returns 3 early (via `leave`) when x > 3, skipping the
    // `+ 100`; otherwise it returns x + 100.
    //   clamp(2) = 102, clamp(9) = 3  =>  102 + 3 = 105
    // A broken `leave` would fall through and add 100 to the x > 3 branch too
    // (clamp(9) => 103 => total 205).
    public function clampSum() -> uint256 {
        let result : word;
        assembly {
            function clamp(x) -> y {
                y := x
                if gt(x, 3) {
                    y := 3
                    leave
                }
                y := add(y, 100)
            }
            result := add(clamp(2), clamp(9))
        }
        return uint256(result);
    }
}
