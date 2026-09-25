export { C, T(*) };

trait C<t> {
    function f(x: t) returns (word);
}

enum T { T }

impl C<T> {
    function f(x: T) returns (word) { return 7; }
}
