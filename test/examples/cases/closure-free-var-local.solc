function test() returns (word) {
    let f = lam (x: word) returns (word) {
        let y : word = 42;
        return y;
    };
    return f(1);
}

contract C {
    function main() public returns (word) {
        return test();
    }
}
