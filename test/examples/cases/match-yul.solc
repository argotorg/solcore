enum Wrapper { Wrapper(word) }
contract C {
    function main() public returns (word) {
        return foo(Wrapper(1));
    }
    function foo(w:Wrapper) public returns (word) {
        let result : word;
        match (w ) {
            case Wrapper(ptr) {
                //let ptr2 : word = ptr;
                assembly { result := calldataload(ptr) }
        } }
        return result;
    }
}
