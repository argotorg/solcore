import {Typedef, uint256} from std;

function wrap(raw: word) returns (uint256) {
    return Typedef.abs(raw);
}

function unwrap(value: uint256) returns (word) {
    return Typedef.rep(value);
}

function identity(value: word) returns (word) {
    return value;
}

function genericWrap<a, rep>(raw: rep) returns (a) where a: Typedef<rep> {
    return Typedef.abs(raw);
}

function genericUnwrap<a, rep>(value: a) returns (rep) where a: Typedef<rep> {
    return Typedef.rep(value);
}

function genericRoundtrip(raw: word) returns (word) {
    let wrapped: uint256 = genericWrap(raw);
    return genericUnwrap(wrapped);
}

contract AsConversion {
    function main(raw: word) public returns (word) {
        return genericRoundtrip(identity(unwrap(wrap(raw))));
    }
}
