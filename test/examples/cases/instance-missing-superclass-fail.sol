// A trait's `where` clause is a superclass constraint that every instance must
// satisfy: `trait B<t> where t: D` means an `impl B<X>` requires an `impl D<X>`.
// Here there is no `impl D<X>` anywhere, so `impl B<X>` must be rejected at the
// impl (SC0233) rather than silently accepted and failing later at a use site.

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
