import std.{*};
import std.dispatch.{*};

contract WithFallback {
    constructor() {}

    public function answer() -> uint256 {
        return uint256(42);
    }

    fallback() -> () {
        revertLit("fallback-was-called");
    }
}
