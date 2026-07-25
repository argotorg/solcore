enum memory<a> { memory(word) }

trait Typedef<abs, rep> {
    function abs(v:rep) returns (abs);
    function rep(v:abs) returns (rep);
}

impl Typedef<a memory, word> {
    function abs(ptr:word) returns (a memory) {
        return memory(ptr);
    }
    function rep(v:a memory) returns (word) {
        match (v ) {
            case memory(ptr) { return ptr;
        } }
    }
}

trait Test<self> {
    function test(x:self) returns (word);
}

impl Test<word> {
    function test(x:word) returns (word) {
        return x;
    }
}

enum test<a> { test(a memory) }

impl Typedef<test<a>, a memory> {
    function rep(x:test<a>) returns (a memory) {
        match (x ) {
            case test(m) { return m;
        } }
    }
    function abs(m:a memory) returns (test<a>) {
        return test(m);
    }
}

impl<abs, rep> Test<test<abs>> where test<abs>: Typedef<rep>, rep: Test {
    function test(x:test<abs>) returns (word) {
        return Test.test(Typedef.rep(x));
    }
  }

contract C {
    function main() public {
        let x:test<word> = test(memory(42));
        let ptr:word = Test.test(x);
    }
}
