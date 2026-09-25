// Companion to instance-missing-superclass-fail.sol: once the required
// superclass instance `impl D<X>` exists, `impl B<X>` is accepted.  The
// superclass impl is declared AFTER the `impl B<X>` that needs it, to confirm
// the check is independent of instance declaration order.

trait D<t> {
    function d(x: t) returns (word);
}

trait B<t> where t: D {
    function b(x: t) returns (word);
}

enum X { X }

impl B<X> {
    function b(x: X) returns (word) { return 1; }
}

impl D<X> {
    function d(x: X) returns (word) { return 2; }
}
