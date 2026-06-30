import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract Bug {
    public function main() -> word {
        return makeClosure(42);
    }

    public function makeClosure(e : word) -> word {
        let f = lam (x : word) {
            return e + x;  // Uses Add.add typeclass method
        };
        return f(1);
    }
}
