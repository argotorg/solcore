# Source syntax

This compiler follows the source grammar implemented by `solcore-rs` on its
`new-syntax` branch (reference revision
`59f11626`, including the parser's existing pragma, export, and type-alias
extensions), with named-field structs carried forward from local `main`.
Sources and imported modules use `.sol`. A module `a.b` maps to
`a/b.sol` relative to the importing module's directory.

Older source spellings are not accepted. In particular, there are no `data`,
`class`, `instance`, or `forall` declarations, postfix type locations or array
types, `as` expressions, named function results, or destructuring `let`
bindings. Interface and library declarations are outside this grammar.

## Modules

Imports and declarations may be interleaved:

```solidity
import std;
import * from std;
import * from std hiding {debug};
import * as math from @vendor.math;
import {foo, bar as baz} from lib.helpers;
```

Plain imports introduce module qualifiers. Wildcard and selective imports open
public names and may have a `hiding` clause. A wildcard is written `*`, not
`{*}`. Selectors and hiding lists may name operators using parentheses, such as
`(+)`. String-path imports and the former `import M.{item}` spelling are rejected.

The reference parser retains these export forms:

```solidity
export {foo, Option(*), Result(Ok), (+)};
export helpers;
export helpers as publicHelpers;
export helpers.{foo, Option(*)};
export helpers.*;
```

## Pragmas

The implemented switches use their hyphenated names, with an optional list of
target identifiers:

```solidity
pragma no-coverage-condition;
pragma no-patterson-condition Trait;
pragma no-bounded-variable-condition Trait;
pragma no-generic-instance-for MyType;
```

The pragma parser also preserves unknown identifier directives. It does not
accept the former `pragma solcore ...` namespace or Solidity version expressions.

## Types and bindings

Named bindings put their name before a colon and type. Type arguments use
nonempty angle-bracket lists; tuple and function types may be empty.

```solidity
word
Option<word>
collections.Map<address, Option<word>>
(word, bool)
mapping(address => word)
array<word>
memory<DynArray<word>>
calldata<array<word>>
storage<mapping(address => word)>
function(word, bool) returns (word)
function(word)
@word
```

`@T` is a type witness in expression position and abbreviates `Proxy<T>` in type
position. Locations are ordinary unary constructors (`memory<T>`, `storage<T>`,
`calldata<T>`); there are no `T memory`, `T[]`, or `T[N]` forms.

```solidity
let count: word = 1;
let inferred = compute();
let output: word;
let witness: @word = @word;
let values: memory<DynArray<uint256>> = [1, 2, 3];
let constant: comptime<word> = 42;
```

Local bindings introduce one identifier. An initializer uses `=`, never `:=`.
Comptime parameters use `comptime name: Type`; comptime locals and results use
`comptime<Type>`.

The parser also accepts `comptime<Type>` inside other types. The current compiler
supports its evaluation mode only on direct local bindings and function results,
alongside the `comptime` parameter modifier. Nested uses, including enum payloads,
type arguments, aliases, and function-type inputs or results, produce a diagnostic
at the unsupported type instead of discarding the compile-time requirement.

Transparent type aliases retain the reference parser's `type` syntax and
parenthesized binders, including inside contracts:

```solidity
type Word = word;
type PairOf(a) = (a, a);
```

## Functions and contracts

Named function parameters require types. Omitting `returns` declares a unit
result. Result entries are positional types; multiple entries form a tuple.

```solidity
function identity<T>(value: T) returns (T) { return value; }
function pair(x: word) returns (word, word) { return (x, x); }
function nop() {}
function increment(x: word) returns (word) { x + 1 }
function constant(comptime x: word) returns (comptime<word>) { return x; }
```

Only a named function may end with an implicit result expression without a
semicolon. Nested blocks, match arms, lambdas, constructors, and fallbacks
require statement terminators.

```solidity
contract Counter {
    value: word;
    constructor(initial: word) payable { value = initial; }
    function read() public returns (word) { return value; }
    function update(next: word) public payable { value = next; }
    fallback() payable { return; }
}
```

Contract functions accept `public`, then `payable`, before `returns` and `where`.
Constructors and fallbacks are implicitly public and accept only `payable`.
Top-level functions and trait/impl methods take neither attribute. There are no
`external`, `internal`, `private`, `pure`, `view`, or `receive` declaration forms.

Lambdas retain `lam`, allow inferred parameters, and use an optional arrow result:

```solidity
let increment = lam (x: word) -> word { return x + 1; };
let identity = lam (x) { return x; };
```

## Enums, structs, traits, and implementations

```solidity
enum Option<T> { None, Some(T) }

trait Eq<T> {
    function eq(left: T, right: T) returns (bool);
}

impl<T> Eq<Option<T>> where T: Eq {
    function eq(left: Option<T>, right: Option<T>) returns (bool) {
        match ((left, right)) {
            case (.None, .None) { return true; }
            case (.Some(x), .Some(y)) { return Eq.eq(x, y); }
            default { return false; }
        }
    }
}
```

The first trait parameter is the instance-head type. Further parameters are
trait arguments. `where T: Convert<U>` constrains `Convert<T, U>`; multiple
constraints are comma-separated and may be enclosed in parentheses.
`default impl` is supported. An enum may have no variants, and each variant may
have positional payload types. `#[derive(...)]` accepts a nonempty list of trait
paths and is supported on top-level and contract-local enums. Explicit derives
use the Generic representation and the existing trait implementations for its
components.

Structs extend the reference grammar with named product fields. They accept the
same angle-bracket type parameters and derive attributes as enums, at top level
or inside contracts. Each field has a type and ends with a semicolon; field names
must be unique within the struct. Constructors take values in field order:

```solidity
#[derive(Eq)]
struct Pair<T> {
    left: T;
    right: T;
}

function first(value: Pair<word>) returns (word) { return value.left; }
function pair() returns (Pair<word>) { return Pair.Pair(1, 2); }
```

A member read evaluates its receiver once. Storage-backed struct fields support
member updates through the storage API. A contract-local struct's type belongs
to its declaring contract, so separate contracts can reuse the same type name.

Constructors use qualified names (`Option.Some(1)`) or expected-type shorthand
(`.Some(1)`); the same forms appear in patterns. Tuple patterns and `_` are
supported in matches. Every match has at least one case/default arm, with
`default` last. A comptime label is written `case comptime expression { ... }`.

## Expressions and statements

Calls, member calls, indexing, tuples, array literals, and ternary expressions
are supported. Unary operators are `!` and `~`; binary operators are `*`, `/`,
`%`, `+`, `-`, `&`, `^`, `|`, comparisons, equality, `&&`, and `||`. Precedence
matches the reference parser; bitwise operators bind more tightly than
comparisons. Comparison and equality operators do not chain. There are no
unary `+`/`-`, shifts, exponentiation, increment/decrement, or `as` expressions.

Statements include `let`, assignment, expressions, return, blocks, `if`,
`while`, `for`, `break`, `continue`, `match`, and `assembly`. Compound assignments
are `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, and unary `~=`. Address
expressions in compound assignments are evaluated once. `for` headers may
contain comma-separated declarations/assignments and omit each header part.
There are no `unchecked` blocks or bare `revert` statements.

Assembly blocks use the reference Yul source grammar. Compiler-internal Yul
quotation templates are not source-language expressions.

## Lexical rules

Identifiers start with a Unicode letter and continue with Unicode letters,
numbers, or underscores. `_` is a wildcard, not a binding name. Hyphenated
identifiers are restricted to pragmas; put spaces around subtraction.
Nested block comments and line comments are supported. Strings use double
quotes and the escapes `\n`, `\t`, `\"`, and `\\`. Integers are decimal or
lowercase-`0x` hexadecimal. List trailing-comma rules follow the reference
parser; ordinary function calls do not permit a trailing argument comma.
