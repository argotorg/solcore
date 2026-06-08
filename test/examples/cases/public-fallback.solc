import std.{*};
import std.dispatch.{*};

contract PublicFallback {
    constructor() {}

    public fallback() -> () {
        revert("fallback-was-called");
    }
}
