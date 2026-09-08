import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(NoSuchClass)]
enum Color { Red, Green, Blue }

function useIt() returns (bool) {
    return true;
}
