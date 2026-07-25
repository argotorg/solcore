import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract Bug {
    function main() public returns (word) {
        return makeClosure(42);
    }

    function makeClosure(e : word) public returns (word) {
        let f = lam (x : word) {
            return e + x;  // Uses Add.add typeclass method
        };
        return f(1);
    }
}
