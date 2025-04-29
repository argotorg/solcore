import "Prelude.sol";

//////// Value Types

data bool = true | false;

instance bool:Typedef(word) {
    function rep(x:bool) -> word {
        match x {
            | true => return 1;
            | false => return 0;
        }
    }
    function abs(x:word) -> bool {
        // TODO
        return true;
    }
}

data uint256 = uint256(word);

instance uint256:Typedef(word) {
    function rep(x:uint256) -> word {
        match x {
            | uint256(w) => return w;
        }
    }
    function abs(x:word) -> uint256 {
        return uint256(x);
    }
}

data bytes32 = bytes32(word);

instance bytes32:Typedef(word) {
    function rep(x:bytes32) -> word {
        match x {
            | bytes32(w) => return w;
        }
    }
    function abs(x:word) -> bytes32 {
        return bytes32(x);
    }
}

data uint8 = uint8(word);

instance uint8:Typedef(word) {
    function rep(x:uint8) -> word {
        match x {
            | uint8(w) => return w;
        }
    }
    function abs(x:word) -> uint8 {
        assembly {
            if gt(x, 0xFF) {
                revert(0,0)
            }
        }
        return uint8(x);
    }
}


/////// Reference Types

data bytes = BytesVoid; // TODO: shouldn't be constructible
data dynamicArray(a) = DynamicArrayVoid;  // TODO: shouldn't be constructible
data staticArray(a, sz) = StaticArrayVoid;  // TODO: shouldn't be constructible

// ...
