import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.ABIGeneric;

pragma solcore noPattersonCondition;
pragma solcore noCoverageCondition;
pragma solcore noBoundVariableCondition;

enum Option<a> { None, Some(a) }

// Manual Generic instance without pragma no-generic-instance-for Option.
// The compiler must reject this with a conflict error.
impl Generic<Option<uint256>, sum<(), uint256>> {
    function from(x : Option<uint256>) returns (sum<(), uint256>) {
        match (x ) {
        case Option.None    { return inl(());
        } case Option.Some(v) { return inr(v);
        } }
    }
    function to(r : sum<(), uint256>) returns (Option<uint256>) {
        match (r ) {
        case inl(_) { return Option.None;
        } case inr(v) { return Option.Some(v);
        } }
    }
}
