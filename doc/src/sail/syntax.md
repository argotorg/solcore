# Syntax

SAIL uses a Solidity-style surface syntax with a statically typed functional
core. This page summarizes the source grammar. The machine-readable EBNF is in
[`doc/railroad/sail.bnf`](../../railroad/sail.bnf).

`[ ... ]` marks an optional element and `{ ... }` marks repetition in grammar
fragments on this page.

---

## Source Files

A source file contains imports, pragmas, and top-level declarations in any
order:

```text
CompilationUnit = { Import | Pragma | TopDecl }
```

Both Classic and Core Solidity use the `.sol` extension in the language
specification. The prototype may temporarily accept `.solc` files.

Identifiers begin with a letter or underscore and may contain letters, decimal
digits, and underscores. Integer literals may be decimal or `0x`-prefixed
hexadecimal values. Strings use double quotes.

---

## Imports

Module paths are dotted names. An external package path begins with
`@package.`.

```solidity
import std;
import std.dispatch;
import * as dispatch from std.dispatch;
import {address, uint256 as U256} from std;
import {foo, bar as baz} from @ext.foo.bar;
```

Core rejects string paths and selector-after-module ordering; selective names
always precede `from`.

The current compiler also supports `import {*} from M` and an optional
`hiding {X, Y}` clause as module-system extensions.

### Exports

The canonical new syntax does not yet select an export or re-export spelling.
The compiler currently retains its existing `export` declarations as an
implementation extension. See [Modules](modules.md) for those provisional
forms.

---

## Pragmas

Solidity and ABI-coder pragmas retain their familiar spelling:

```solidity
pragma solidity ^0.8.23;
pragma abicoder v2;
```

Solcore-specific pragmas use the `solcore` namespace:

```solidity
pragma solcore noCoverageCondition;
pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;
pragma solcore noGenericInstanceFor MyType;
```

---

## Types

Named and generic types use dotted names and angle brackets:

```solidity
word
pkg.Option<word>
collections.Map<address, pkg.Option<word>>
```

Other type forms are:

```solidity
mapping(address => word)
word[]
word[4]
(word, bool)
()
function(word) internal returns (bool)
bytes memory
bytes calldata
```

Array suffixes and the data locations `memory`, `storage`, and `calldata`
follow the complete element type. Function types use `function(...)` and
`returns (...)`; the former source-level arrow type is not part of the grammar.

Explicit conversion uses `as` with a complete target type:

```solidity
let n = raw as word;
let callback = value as function(word) internal returns (bool);
let result = value as pkg.Result<word, Error>;
```

There is no general `expression: Type` annotation form. Use a typed binding
when an expression needs an expected type:

```solidity
let value: T = expression;
```

---

## Structs, Enums, and Type Declarations

Struct fields use name-first declarations:

```solidity
struct Pair {
    x: word;
    y: word;
}
```

Ordinary enums and payload-carrying algebraic data types share one declaration
form:

```solidity
enum Status {
    Pending,
    Filled,
    Cancelled
}

enum Option<T> {
    None,
    Some(T)
}
```

Constructors are qualified in expressions and patterns:

```solidity
Option.Some(1)
Option.None
```

A user-defined type uses `is`:

```solidity
type Wad is word;
```

---

## Traits, Implementations, and Generics

Type classes use `trait`; implementations use `impl`. Generic parameters
follow the declared name in angle brackets, and constraints follow the head in
a `where` clause.

```solidity
trait Eq<T> {
    function eq(x: T, y: T) returns (bool);
}

impl Eq<word> {
    function eq(x: word, y: word) returns (bool) {
        return x == y;
    }
}

impl<T> Eq<Option<T>> where T: Eq {
    function eq(x: Option<T>, y: Option<T>) returns (bool) {
        return true;
    }
}
```

The compiler also accepts `default impl` as an implementation-selection
extension. Legacy generic and type-class declaration spellings are not source
syntax.

---

## Contracts and Fields

Contracts, interfaces, and libraries use Solidity-style shells. Every named
field and parameter places the name before its type.

```solidity
contract Token {
    balances: mapping(address => word);

    constructor(initialSupply: word) payable {
        balances[msg.sender] = initialSupply;
    }

    function balanceOf(account: address) public returns (word) {
        return balances[account];
    }

    fallback() external payable {
        // Handle unmatched selectors.
    }
}
```

The initial Core surface has one general `fallback` entry point and no separate
`receive`. A fallback must be `external`; it may also be `payable`.

Interfaces contain semicolon-terminated function signatures, while libraries
contain fields, structs, enums, and function definitions:

```solidity
interface Hashable {
    function hash(value: word) external returns (word);
}

library Hashing {
    function hash(value: word) internal returns (word) {
        return value;
    }
}
```

---

## Functions

Function parameters are name-first. Attributes follow the parameter list, and
results use `returns (...)`.

```solidity
function addOne(x: word) pure returns (word) {
    return x + 1;
}

function pair() returns (word, word) {
    return (1, 2);
}

function namedResult() returns (result: word) {
    return 1;
}

function nop() {
    return;
}
```

Generic parameters follow the function name. Constraints appear after the
return clause.

```solidity
function id<T>(x: T) returns (T) {
    return x;
}

function eqSelf<T>(x: T) returns (bool) where T: Eq {
    return Eq.eq(x, x);
}
```

`comptime` immediately precedes the binding it modifies:

```solidity
function pow(comptime n: word, x: word) returns (word) {
    let comptime exponent = n;
    return x ** exponent;
}
```

---

## Local Bindings and Statements

Local variables use `let`, with or without an explicit type or initializer:

```solidity
let amount: word = readAmount();
let owner: address;
let inferred = computeValue();
let (left, right): (word, bool) = readResult();
```

Statements use semicolon terminators where shown:

```solidity
return;
return value;
if (condition) { ... } else { ... }
for (let i: word = 0; i < n; i = i + 1) { ... }
while (condition) { ... }
break;
continue;
unchecked { ... }
assembly { ... }
revert;
```

Assignments support `=`, compound assignment operators, field access, and
indexing. A plain call or other expression used as a statement also ends in
`;`.

---

## Pattern Matching

`match` encloses one or more scrutinees in parentheses. Each arm has its own
block.

```solidity
match (value) {
    case Option.Some(x) {
        return x;
    }
    case Option.None {
        return 0;
    }
}

match (x, y) {
    case (Option.Some(a), Option.Some(b)) {
        return a + b;
    }
    default {
        return 0;
    }
}
```

The compiler extension `.Constructor` is available when an expected type makes
the constructor family unambiguous.

---

## Expressions

Expressions include literals, names, tuples, calls, field access, indexing,
unary and binary operators, conditional expressions, and conversions:

```solidity
f(x, y)
token.balanceOf(account)
values[index]
!ok
x ** exponent
x * y + z
x << bits
x & mask
x == y
condition ? yes : no
expression as T
```

Power is right-associative. Multiplication and addition, shifts, comparisons,
equality, bitwise operators, logical operators, and the conditional operator
then follow in decreasing precedence. Conversion with `as` binds more tightly
than power and is left-associative.

The compiler retains `lam(...) returns (...) { ... }` for lambda expressions as
a Core extension.

---

## Assembly

An `assembly { ... }` block embeds the Yul sublanguage. Yul declarations,
assignment, `if`, `switch`, and `for` retain Yul syntax and do not use SAIL
statement terminators.

```solidity
function load(slot: word) returns (word) {
    let value: word;
    assembly {
        value := sload(slot)
    }
    return value;
}
```

Only surrounding values represented as `word` may be referenced directly from
Yul.
