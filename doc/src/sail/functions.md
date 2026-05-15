# Functions

A function definition introduces a named computation that takes zero or more
typed parameters and returns a value of a declared type. Functions can be
defined at the top level of a source file, called _free functions_, or inside
a contract body.

```solidity
function name(param1 : Type1, param2 : Type2) -> ReturnType {
    // body
}
```

Every top-level function must carry a complete type signature: every parameter
must be annotated with its type, and the return type must be provided after
`->`. The compiler rejects any top-level definition that omits an annotation.

> **Note** The complete-annotation requirement applies to free functions and
> contract methods. It does not apply to lambda expressions or to local
> bindings inside a function body, where the compiler infers types from
> context.

---

## Parameters

Parameters are declared as a comma-separated list enclosed in parentheses.
Each parameter has the form `name : Type`.

```solidity
function transfer(to : word, amount : word) -> () {
    let bal : word;
    assembly { bal := sload(caller()) }
    assembly { sstore(caller(), sub(bal, amount)) }
    assembly { sstore(to, add(sload(to), amount)) }
}
```

A function that takes no arguments is written with an empty parameter list:

```solidity
function sender() -> word {
    let s : word;
    assembly { s := caller() }
    return s;
}
```

---

## Return Type

The return type follows the parameter list after `->`. Every top-level function
must declare its return type explicitly.

A function that returns no meaningful value uses the unit type `()`:

```solidity
function emitTransfer(from : word, to : word, amount : word) -> () {
    assembly {
        mstore(0x00, amount)
        log3(0x00, 0x20, 0xddf252ad, from, to)
    }
}
```

Every execution path through the body must end with a `return` statement whose
expression has the declared return type.

---

## Free Functions

A function defined outside any contract body is called a _free function_. Free
functions are visible throughout the file in which they are defined and can be
imported by other modules.

```solidity
function isContract(addr : word) -> bool {
    let size : word;
    assembly { size := extcodesize(addr) }
    return gt(size, 0);
}

contract Token {
    function onlyContract(addr : word) -> () {
        if (isContract(addr)) {
            return ();
        } else {
            assembly { revert(0, 0) }
        }
    }
}
```

---

## Polymorphic Functions

A function that works uniformly over multiple types can be made polymorphic
with a `forall` quantifier placed before the `function` keyword. The quantifier
lists the type variables that appear in the signature.

```solidity
forall a . function identity(x : a) -> a {
    return x;
}

forall a b . function fst(p : (a, b)) -> a {
    match p {
    | (x, y) => return x;
    }
}
```

Type variables introduced by `forall` are instantiated at each call site. The
compiler specializes the function for every concrete type combination that
appears in the program.

> **Note** Polymorphic functions are monomorphized by the specializer before
> code generation. Each distinct instantiation produces a separate function in
> the output. A call to `identity` with a `word` argument becomes
> `identity$word` in the compiled output. No polymorphism survives to the
> generated Yul.

---

## Constrained Functions

A function may require that one or more of its type variables satisfy a type
class constraint. Constraints are written after the type variable list,
separated from the `function` keyword by `=>`.

```solidity
forall a . class a:Checked {
    function checkedAdd(x : a, y : a) -> a;
}

forall t . t:Checked => function safeTransfer(from : word, to : word, amount : t) -> t {
    return Checked.checkedAdd(amount, amount);
}
```

Multiple constraints on different type variables are separated by commas:

```solidity
forall a b . a:Eq, b:Eq => function transfersEqual(x : (a, b), y : (a, b)) -> bool {
    match (x, y) {
    | ((xa, xb), (ya, yb)) => return Eq.eq(xa, ya);
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
function sumBalances(slot : word, count : word) -> word {
    if (eq(count, 0)) {
        return 0;
    } else {
        let bal : word;
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
data TxStatus = Pending | Confirmed;

function isPending(s : TxStatus) -> bool {
    match s {
    | TxStatus.Pending   => return isNotConfirmed(s);
    | TxStatus.Confirmed => return false;
    }
}

function isNotConfirmed(s : TxStatus) -> bool {
    match s {
    | TxStatus.Confirmed => return isPending(TxStatus.Pending);
    | TxStatus.Pending   => return true;
    }
}
```

---

## Contract Functions

Functions defined inside a contract body have access to the contract's field
variables. They follow the same signature rules as free functions.

```solidity
contract ERC20 {
    totalSupply : word;

    function mint(amount : word) -> () {
        totalSupply = add(totalSupply, amount);
    }

    function getTotalSupply() -> word {
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
data Result = Ok(word) | Err(word);

function unwrapOrZero(r : Result) -> word {
    match r {
    | Result.Ok(v)  => return v;
    | Result.Err(_) => return 0;
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
function loadBalance(account : word) -> word {
    let bal : word;
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

Omitting a parameter type or the return type on a top-level function is a
compile-time error. The compiler reports the offending signature and explains
what is missing.

```solidity
// Error: parameter 'x' has no type annotation.
function bad(x) -> word {
    return x;
}
```

```
Top-level function must have complete type annotations:
  bad(x) -> word
Annotate every parameter (name : Type) and provide a return type (-> Type).
```

Omitting the return type is equally rejected:

```solidity
// Error: return type is missing.
function alsobad(x : word) {
    return x;
}
```

Type inference remains available inside function bodies for local variables and
intermediate expressions. Only the function signature itself requires explicit
annotations at the top level.
