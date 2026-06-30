data Wrapper = Wrapper(word);
contract C {
    public function main() -> word {
        return foo(Wrapper(1));
    }
    public function foo(w:Wrapper) -> word {
        let result : word;
        match w {
            | Wrapper(ptr) =>
                //let ptr2 : word = ptr;
                assembly { result := calldataload(ptr) }
        }
        return result;
    }
}
