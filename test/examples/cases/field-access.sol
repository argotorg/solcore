import {*} from std;

contract PoC {
    field : word;

    function set_x(b: bool) public returns (bool) {
        field = b;   // BUG: `word` shouldn't be unified with `bool`.
        return b;
    }

    function init(foo: bool) public returns (()) {
       field = 2;
    }

    function main () public returns (()) {

    }
}
