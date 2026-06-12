import std.{*};
import std.dispatch.{*};

contract PayableTest {
    constructor() {}

    public payable function deposit() -> uint256 {
        let value;
        assembly {
            value := callvalue()
        }
        return uint256(value);
    }

    public function balance() -> uint256 {
        let value;
        assembly {
            value := selfbalance()
        }
        return uint256(value);
    }

    payable fallback() -> () {
        let value;
        assembly {
            value := callvalue()
        }
        if (value == 0) {
            revertLit("fallback-was-called-no-value");
        }
    }
}
