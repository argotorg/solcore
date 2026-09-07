import {*} from std;
import {*} from std.dispatch;

contract PublicFallback {
    constructor() {}

    fallback() external public  {
        revert("fallback-was-called");
    }
}
