# Numeric Types

Core Solidity's scalar types are not compiler built-ins. Each one is an ordinary
single-constructor data type that wraps the machine word `word`, the 256-bit
untyped value described in [Built-ins](../sail/builtins.md). The wrapping is
established through the `Typedef` trait, which relates an _abstract_ type to its
_representation_ type:

```solidity
trait Typedef<abs, rep> {
    function abs(x: rep) returns (abs);
    function rep(x: abs) returns (rep);
}
```

For example, `uint256` is defined, in full, as:

```solidity
enum uint256 { uint256(word) }

impl Typedef<uint256, word> {
    function abs(w: word) returns (uint256) { return uint256(w); }
    function rep(x: uint256) returns (word) { return x; }  // elides the match
}
```

`Typedef.abs` injects a raw word into the abstract type and `Typedef.rep`
projects it back out. Because the constructor is the only inhabitant and carries
exactly one word, the wrapper has no runtime cost: after specialization a
`uint256` _is_ a word on the stack (see
[Runtime Encoding](../sail/datatypes.md#runtime-encoding)). The wrapper exists
only so that the type checker can keep `uint256`, `address`, and a raw `word`
distinct, and so that each can carry its own trait instances.

## The scalar types

The standard library ships the following word-backed types. All are exported
from `std`.

| Type      | Definition                | Meaning                                          |
| --------- | ------------------------- | ------------------------------------------------ |
| `word`    | built-in                  | the raw, untyped 256-bit EVM word                |
| `bool`    | built-in (`true`/`false`) | a boolean                                        |
| `uint256` | `enum { uint256(word) }`  | an unsigned 256-bit integer                      |
| `address` | `enum { address(word) }`  | a 160-bit account address (upper bits clear)     |
| `bytes4`  | `enum { bytes4(word) }`   | four bytes, right-aligned (e.g. a selector)      |
| `bytes32` | `enum { bytes32(word) }`  | a full 32-byte word (e.g. a hash)                |

`word` and `bool` are the only two that are genuinely primitive; the type
checker knows them directly. The rest are library data types. A value is moved
between a scalar and its underlying word with `Typedef.abs` / `Typedef.rep`, or
with the named constructor used as a conversion:

```solidity
function selectorOf(h: bytes32) returns (bytes4) {
    return bytes4(shr(224, Typedef.rep(h)));
}
```

> **Note** The prototype ships only the 256-bit unsigned integer. Narrower and
> signed integer families (`uint8` … `uint128`, `int8` … `int256`) follow the
> same `enum T { T(word) }` pattern, differing only in how each operation
> reduces its result back into range, and are planned as generated standard
> library modules.

## Trait membership

Each scalar type participates in the standard numeric traits, which give it
equality, ordering, bounds, and arithmetic. The umbrella trait is `Num`:

```solidity
trait Num<a> where a: Add, a: Sub, a: Bounded, a: Eq, a: Ord, a: Typedef<word> {
    function maxVal() returns (a);
    function toWord(x: a) returns (word);
    function fromWord(x: word) returns (a);
    function fromInteger(comptime x: integer) returns (comptime<a>);
    function add(x: a, y: a) returns (a);
    function sub(x: a, y: a) returns (a);
    function gt(x: a, y: a) returns (bool);
}
```

A default instance derives `Num` for any type that is a `Typedef` over `word`
and implements the component traits, so a new word-backed type becomes numeric
with no hand-written `Num` instance. The operators that these traits back are
covered in [Arithmetic](./arithmetic.md).

## Integer literals

A bare integer literal such as `42` has the compile-time type `integer`, an
_unbounded_ mathematical integer with no runtime representation (see
[Comptime Integers](../sail/builtins.md)). It becomes a runtime value through
`Num.fromInteger`, which the compiler inserts automatically wherever a literal
is used at a concrete numeric type:

```solidity
function fee() returns (uint256) {
    return 42;          // elaborated to Num.fromInteger(42) : uint256
}
```

Because `fromInteger` takes a `comptime integer`, the conversion and any
constant-folding happen during compile-time evaluation; the deployed bytecode
contains the final word, not the literal-to-value conversion. A literal that
does not fit the target type is therefore a compile-time error rather than a
silent wrap. The same mechanism lets a single literal denote a value of any
numeric type, with the target type chosen by inference.
