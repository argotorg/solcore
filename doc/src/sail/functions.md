# Functions

A function definition introduces a named computation that takes zero or more
typed parameters and returns a value of a declared type. Functions can be
defined at the top level of a source file, called _free functions_, or inside
a contract body.

```solidity
function name(param1: Type1, param2: Type2) returns (ReturnType) {
    // body
}
```

Every top-level function must carry a complete type signature: every parameter
must be annotated with its type, and a value-producing function declares its
result in a `returns (...)` clause. A function with no `returns` clause has the
unit result type. The compiler rejects any top-level definition that leaves a
parameter unannotated.

> **Note** The complete-annotation requirement applies to free functions and
> contract methods. It does not apply to lambda expressions or to local
> bindings inside a function body, where the compiler infers types from
> context.

---

## Parameters

Parameters are declared as a comma-separated list enclosed in parentheses.
Each parameter has the name-first form `name: Type`.

```solidity
function transfer(to: word, amount: word) {
    let bal: word;
    assembly { bal := sload(caller()) }
    assembly { sstore(caller(), sub(bal, amount)) }
    assembly { sstore(to, add(sload(to), amount)) }
}
```

A function that takes no arguments is written with an empty parameter list:

```solidity
function sender() returns (word) {
    let s: word;
    assembly { s := caller() }
    return s;
}
```

---

## Return Type

Return types follow the parameter list in a `returns (...)` clause. Multiple
result types are separated by commas.

A function that returns no meaningful value omits the clause:

```solidity
function emitTransfer(from: word, to: word, amount: word) {
    assembly {
        mstore(0x00, amount)
        log3(0x00, 0x20, 0xddf252ad, from, to)
    }
}
```

Every execution path through a value-producing body must end with a `return`
statement whose expression has the declared return type. A bare `return;`
returns unit.

---

## Free Functions

A function defined outside any contract body is called a _free function_. Free
functions are visible throughout the file in which they are defined and can be
imported by other modules.

```solidity
function isContract(addr: word) returns (bool) {
    let size: word;
    assembly { size := extcodesize(addr) }
    return gt(size, 0);
}

contract Token {
    function onlyContract(addr: word) {
        if (isContract(addr)) {
            return;
        } else {
            assembly { revert(0, 0) }
        }
    }
}
```

---

## Polymorphic Functions

A function that works uniformly over multiple types can be made polymorphic
by listing generic type parameters in angle brackets after the function name.

```solidity
function identity<A>(x: A) returns (A) {
    return x;
}

function fst<A, B>(p: (A, B)) returns (A) {
    match (p) {
        case (x, y) {
            return x;
        }
    }
}
```

Generic type parameters are instantiated at each call site. The compiler
specializes the function for every concrete type combination that appears in
the program.

> **Note** Polymorphic functions are monomorphized by the specializer before
> code generation. Each distinct instantiation produces a separate function in
> the output. A call to `identity` with a `word` argument becomes
> `identity$word` in the compiled output. No polymorphism survives to the
> generated Yul.

---

## Constrained Functions

A function may require that one or more of its type variables satisfy a type
class constraint. Constraints are written after the return clause in a `where`
clause.

```solidity
trait Checked<A> {
    function checkedAdd(x: A, y: A) returns (A);
}

function safeTransfer<T>(from: word, to: word, amount: T)
    returns (T)
    where T: Checked
{
    return Checked.checkedAdd(amount, amount);
}
```

Multiple constraints on different type variables are separated by commas:

```solidity
function transfersEqual<A, B>(x: (A, B), y: (A, B))
    returns (bool)
    where A: Eq, B: Eq
{
    match ((x, y)) {
        case ((xa, xb), (ya, yb)) {
            return Eq.eq(xa, ya);
        }
    }
}
```

At each call site the compiler checks that the supplied types satisfy all
listed constraints. If no instance is found a type error is reported.

---

## Recursive Functions

A function may call itself recursively. The compiler adds the function name to
the typing context before checking the body.

```solidity
function sumBalances(slot: word, count: word) returns (word) {
    if (eq(count, 0)) {
        return 0;
    } else {
        let bal: word;
        assembly { bal := sload(slot) }
        return add(bal, sumBalances(add(slot, 1), sub(count, 1)));
    }
}
```

Mutually recursive functions are also supported. The compiler detects mutual
dependencies automatically through strongly-connected-component analysis and
type-checks the group as a unit. Both functions must be defined in the same
file.

```solidity
enum TxStatus {
    Pending,
    Confirmed
}

function isPending(s: TxStatus) returns (bool) {
    match (s) {
        case TxStatus.Pending {
            return isNotConfirmed(s);
        }
        case TxStatus.Confirmed {
            return false;
        }
    }
}

function isNotConfirmed(s: TxStatus) returns (bool) {
    match (s) {
        case TxStatus.Confirmed {
            return isPending(TxStatus.Pending);
        }
        case TxStatus.Pending {
            return true;
        }
    }
}
```

---

## Contract Functions

Functions defined inside a contract body have access to the contract's field
variables. They follow the same signature rules as free functions.

```solidity
contract ERC20 {
    totalSupply: word;

    function mint(amount: word) {
        totalSupply = add(totalSupply, amount);
    }

    function getTotalSupply() returns (word) {
        return totalSupply;
    }
}
```

Contract functions may read and write field variables. Free functions can only
operate on their parameters and locally declared variables.

---

## Pattern Matching in Function Bodies

Functions may use `match` statements to deconstruct algebraic data type values.

```solidity
enum Result {
    Ok(word),
    Err(word)
}

function unwrapOrZero(r: Result) returns (word) {
    match (r) {
        case Result.Ok(v) {
            return v;
        }
        case Result.Err(_) {
            return 0;
        }
    }
}
```

Patterns may be nested arbitrarily. The wildcard pattern `_` matches any value
without binding it. The compiler checks that the set of patterns covers all
possible constructors of the scrutinee type and reports an error for incomplete
matches.

---

## Assembly in Function Bodies

Functions may contain `assembly` blocks to access EVM opcodes directly. Inside
an assembly block, Yul syntax is used. Variables declared in the surrounding
SAIL scope are accessible by name inside the block.

```solidity
function loadBalance(account: word) returns (word) {
    let bal: word;
    assembly {
        bal := sload(account)
    }
    return bal;
}
```

Variables assigned inside an assembly block must be declared with `let` in the
enclosing SAIL scope before the block opens. The type of such variables must be
`word`, since Yul operates exclusively on 256-bit machine words.

> **Warning** The type checker cannot verify the semantic correctness of Yul
> code. Incorrect assembly can produce contracts that silently compute wrong
> results or revert unexpectedly. Minimize the size of assembly blocks and
> document any non-obvious invariants.

---

## Missing Annotation Error

Omitting a parameter type on a top-level function is a compile-time error. The
compiler reports the offending signature and explains what is missing.

```solidity
// Error: parameter 'x' has no type annotation.
function bad(x) returns (word) {
    return x;
}
```

```
Top-level function must have complete type annotations:
  function bad(x) returns (word)
Annotate every parameter (name: Type).
```

Type inference remains available inside function bodies for local variables and
intermediate expressions. Only the function signature itself requires explicit
annotations at the top level.
