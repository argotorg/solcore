import {*} from std;
import {*} from std.dispatch;

contract BadFallback {
    constructor() {}

    fallback(x: uint256) external  {
        revert("fallback-was-called");
    }
}
