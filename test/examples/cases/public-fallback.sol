import * from std;
import * from std.dispatch;

contract PublicFallback {
    constructor() {}

    fallback()  public  {
        revert("fallback-was-called");
    }
}
