import {*} from std;
import {*} from std.dispatch;

contract WithFallback {
    constructor() {}

    function answer() public returns (uint256) {
        return uint256(42);
    }

    fallback() external  {
        revertLit("fallback-was-called");
    }
}
