import {*} from std;
import {*} from std.dispatch;

contract PayableTest {
    constructor() {}

    function deposit() public payable returns (uint256) {
        let value;
        assembly {
            value := callvalue()
        }
        return uint256(value);
    }

    function balance() public returns (uint256) {
        let value;
        assembly {
            value := selfbalance()
        }
        return uint256(value);
    }

    fallback() external payable  {
        let value;
        assembly {
            value := callvalue()
        }
        if (value == 0) {
            revertLit("fallback-was-called-no-value");
        }
    }
}
