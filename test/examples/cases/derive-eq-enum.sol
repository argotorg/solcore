import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq, Ord)]
enum Color { Red, Green, Blue }

function sameColor() returns (bool) {
    return Eq.eq(Color.Red, Color.Red);
}

function diffColor() returns (bool) {
    return ne(Color.Red, Color.Blue);
}

function ordering() returns (bool) {
    match (Ord.gt(Color.Green, Color.Red) ) {
    case true  { return not(Ord.gt(Color.Red, Color.Green));
    } case false { return false;
    } }
}
