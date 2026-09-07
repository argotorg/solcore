import {*} from std;

function yul_asm_switch_body() returns (()) {
    let result : word = 0;
    let flag : word = 1;
    assembly {
        switch flag
        case 0 { result := 0 }
        default { result := callvalue() }
    }
}

contract Foo {
    function main() public returns (()) {
        return yul_asm_switch_body();
    }
}
