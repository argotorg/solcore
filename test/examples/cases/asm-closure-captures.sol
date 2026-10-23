function calculate(x: word) returns (word) {
    let f = lam () {
        let result: word;
        assembly {
            { let x := 9 }
            if 1 { x := add(x, 1) }
            for { let i := 0 } lt(i, 1) { i := add(i, 1) } {
                x := add(x, 1)
            }
            result := x
        }
        return result;
    };
    return f();
}

contract AssemblyClosureCaptures {
    function main() public returns (word) {
        return calculate(40);
    }
}
