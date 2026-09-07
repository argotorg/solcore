enum memory<a> { memory(word) }

trait Typedef<abs, rep> {
    function abs(v:rep) returns (abs);
    function rep(v:abs) returns (rep);
}

impl Typedef<memory<a>, word> {
    function abs(ptr:word) returns (memory<a>) {
        return memory(ptr);
    }
    function rep(v:memory<a>) returns (word) {
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

enum test<a> { test(memory<a>) }

impl Typedef<test<a>, memory<a>> {
    function rep(x:test<a>) returns (memory<a>) {
        match (x ) {
            case test(m) { return m;
        } }
    }
    function abs(m:memory<a>) returns (test<a>) {
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
