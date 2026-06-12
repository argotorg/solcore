function test() -> word {
    let f = lam (x: word) -> word {
        let y : word = 42;
        return y;
    };
    return f(1);
}

contract C {
    public function main() -> word {
        return test();
    }
}
