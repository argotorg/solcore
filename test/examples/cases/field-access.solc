import std.{*};

contract PoC {
    field : word;

    public function set_x(b: bool) -> bool {
        field = b;   // BUG: `word` shouldn't be unified with `bool`.
        return b;
    }

    public function init(foo: bool) -> () {
       field = 2;
    }

    public function main () -> () {

    }
}
