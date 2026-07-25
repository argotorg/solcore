pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;

export { Generic };

import {*} from std;

// MPTC: isomorphism between a user type and its SOP representation.
// The representation 'rep' is built from primitive Solcore types:
//   sum(f, g)  with constructors inl / inr
//   (f, g)     pair (product)
//   ()         unit
trait Generic<a, rep> {
    function from(x : a) returns (rep);
    function to(x : rep) returns (a);
}
