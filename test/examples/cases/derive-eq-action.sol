import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq, Ord)]
enum Action { Transfer(word, word), Approve(word) }

function sameTransfer() returns (bool) {
    return Eq.eq(Action.Transfer(1, 100), Action.Transfer(1, 100));
}

function transferLtApprove() returns (bool) {
    return Ord.gt(Action.Approve(1), Action.Transfer(1, 100));
}

// Within the same constructor fields compare left to right.
function amountsCompare() returns (bool) {
    return Ord.gt(Action.Transfer(1, 100), Action.Transfer(1, 50));
}
