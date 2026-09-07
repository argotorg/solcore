import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;
function foo(x: word, y: word) returns (word) {
    return x % y;
}
