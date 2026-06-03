import std.{*};
import std.dispatch.{*};

contract BadFallback {
    constructor() {}

    fallback(x: uint256) -> () {
        revert("fallback-was-called");
    }
}
