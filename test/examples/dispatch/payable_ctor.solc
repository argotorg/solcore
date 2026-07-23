import {*} from std;
import {*} from std.dispatch;

// A contract whose constructor is explicitly marked `payable`.
// Deploying it with an incoming value transfer must succeed and the
// transferred value is retained by the newly created contract.
contract PayableCtor {
    constructor() payable {}

    function balance() public returns (uint256) {
        let value;
        assembly {
            value := selfbalance()
        }
        return uint256(value);
    }
}
