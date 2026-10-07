# Arithmetic

Arithmetic, comparison, and bitwise operators are not primitive syntax. Each
operator is sugar for a call to a method of a standard-library trait, dispatched
on the type of its operands. Writing `a + b` is exactly writing `Add.add(a, b)`;
the type checker resolves the instance the same way it resolves any other trait
method (see [Type Classes](../sail/typeclasses.md)).

## Operators and their traits

| Operator  | Desugars to     | Trait    |
| --------- | --------------- | -------- |
| `a + b`   | `Add.add(a, b)` | `Add`    |
| `a - b`   | `Sub.sub(a, b)` | `Sub`    |
| `a * b`   | `Mul.mul(a, b)` | `Mul`    |
| `a / b`   | `Div.div(a, b)` | `Div`    |
| `a % b`   | `Mod.mod(a, b)` | `Mod`    |
| `a & b`   | `BitAnd.band(a, b)` | `BitAnd` |
| `a \| b`  | `BitOr.bor(a, b)`   | `BitOr`  |
| `a ^ b`   | `BitXor.bxor(a, b)` | `BitXor` |
| `~a`      | `BitNot.bnot(a)`    | `BitNot` |
| `a == b`  | `Eq.eq(a, b)`   | `Eq`     |
| `a != b`  | `ne(a, b)`      | `Eq`     |
| `a > b`, `a < b`, `a >= b`, `a <= b` | `Ord.gt` / `lt` / `ge` / `le` | `Ord` |

Each trait has a single method over a single type, for instance:

```solidity
trait Add<t> { function add(l: t, r: t) returns (t); }
trait Mul<t> { function mul(l: t, r: t) returns (t); }
```

Because dispatch follows the operand type, the same `+` serves every numeric
type, and a user type becomes addable simply by implementing `Add`. The string
type, for example, implements `Add` so that `a + b` on strings is concatenation.

## Comparison and ordering

`Eq` provides `eq`; the free function `ne` is its negation. `Ord` requires `Eq`
as a superclass and provides `gt`, with `lt`, `ge`, and `le` derived from it:

```solidity
trait Eq<a> { function eq(x: a, y: a) returns (bool); }
trait Ord<a> where a: Eq { function gt(x: a, y: a) returns (bool); }
```

Comparisons return `bool`. The boolean connectives `&&` and `||` are built-in
conditional forms with short-circuit shape: the right operand is only evaluated
when the left does not already determine the result.

## Bounds

`Bounded` records the smallest and largest value of a type, which the overflow-
free parts of the library and user code use for range checks:

```solidity
trait Bounded<t> {
    function minVal() returns (t);
    function maxVal() returns (t);
}
```

For an unsigned type `minVal` is `0` and `maxVal` is the all-ones word; the free
function `maxVal<t>()` is a convenience wrapper over `Bounded.maxVal`.

## Current semantics: unchecked word arithmetic

The arithmetic instances for `uint256` are currently **unchecked**: they operate
directly on the underlying word with the EVM's wrapping `add`, `mul`, `sub`, and
friends, so results wrap modulo 2²⁵⁶ rather than reverting on overflow. This
matches Solidity's `unchecked { … }` blocks, not its default checked arithmetic.

```solidity
// with the current instances, this returns 0, it does not revert
function wrap() returns (uint256) {
    return maxVal() + 1;
}
```

> **Note** Checked arithmetic (reverting on overflow, as Classic Solidity does
> by default outside `unchecked`) is planned. Because the operators are ordinary
> trait methods, the change is a matter of swapping in checked instances; no
> syntax or compiler change is required. Division and modulo by zero follow the
> EVM, which yields zero rather than reverting, so guard divisors explicitly
> where that matters.

## Working below the abstraction

When a computation genuinely needs raw word behavior, the standard library
exports word-level helpers (`addWord`, `eqWord`, `gtWord`, `bandWord`, and so
on) that operate directly on `word`, and an `assembly` block gives access to the
EVM opcodes themselves (see [Assembly Blocks](../sail/assembly.md)). These are
the building blocks the typed instances above are written in terms of.
