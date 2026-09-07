import {*} from std;
import {*} from std.dispatch;

contract BadFallback {
    constructor() {}

    fallback() external returns (uint256) {
        return uint256(0);
    }
}
