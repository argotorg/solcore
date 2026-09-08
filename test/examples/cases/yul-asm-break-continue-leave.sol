import * from std;

function yul_asm_break_continue_leave() returns (()) {
    let result : word = 0;
    assembly {
        function clamp(x) -> y {
            y := x
            if gt(x, 3) {
                y := 3
                leave
            }
        }
        for { let i := 0 } lt(i, 10) { i := add(i, 1) } {
            if lt(i, 2) {
                continue
            }
            if gt(i, 5) {
                break
            }
            result := add(result, clamp(i))
        }
    }
}

contract Foo {
    function main() public returns (()) {
        return yul_asm_break_continue_leave();
    }
}
