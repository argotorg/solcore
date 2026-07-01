# Type Inference

SAIL uses a constraint-based bidirectional type inference algorithm. The
algorithm divides type information into two flows: a _bottom-up_ flow that
generates a type from an expression, and a _top-down_ flow that pushes an
expected type down into an expression. Both flows run simultaneously through
unification, which merges type information from different sources into a
consistent solution.

The scope of inference is deliberately limited. Top-level function signatures
require complete explicit annotations. Inference operates freely inside
function bodies: local variable types, intermediate expression types, and the
types of arguments to generic functions are all inferred automatically without
any annotation.

---

## What Requires Annotations

Every top-level function must carry a complete type signature: every parameter
must be annotated and the return type must be declared. The compiler rejects
any top-level definition that omits an annotation. This rule applies to free
functions and to functions defined inside a contract body.

```solidity
// Required: every parameter and the return type are annotated.
function transfer(to : word, amount : word) -> () {
    let bal : word;
    assembly { bal := sload(caller()) }
    assembly { sstore(caller(), sub(bal, amount)) }
    assembly { sstore(to, add(sload(to), amount)) }
}
```

For a full description of the annotation requirement and the error it produces,
see [Functions](functions.md).

---

## What Is Inferred

Inside a function body, the compiler infers types for:

- Local variables declared with `let`, whether or not they carry an annotation.
- Intermediate expressions and subexpressions.
- Type arguments to polymorphic function calls.
- Type class constraints required by the body.

The inferred types are propagated through unification. Whenever the type of an
expression is used in a position where another type is expected, the two are
unified: the algorithm finds the most general substitution that makes them
equal, or reports an error if no such substitution exists.

---

## Local Variable Inference

A `let` declaration without a type annotation introduces a fresh type variable.
The compiler assigns a concrete type to that variable as soon as enough
information is available from the surrounding context.

### Inferred from initialiser

When a `let` declaration includes an initialiser, the type is taken from the
initialiser expression. Integer literals always have type `word`.

```solidity
function demo() -> word {
    let amount = 100;     // amount : word, from the integer literal
    let flag   = true;    // flag : bool, from the boolean literal
    return amount;
}
```

### Inferred from subsequent use

When no initialiser is present, the type is inferred from the first use of the
variable.

```solidity
function loadBalance(account : word) -> word {
    let bal : word;           // annotated; no inference needed
    assembly { bal := sload(account) }
    return bal;
}

function compute(account : word) -> word {
    let x;                    // no annotation, no initialiser
    x = sload(account);       // first assignment: x : word
    return x;
}
```

### Inferred from return context

A variable whose type depends on an algebraic data type can have its type fixed
by the expected return type.

```solidity
data Result = Ok(word) | Err(word);

function demo() -> Result {
    let x = Result.Err(0);     // x : Result, inferred from constructor
    return x;
}
```

---

## Contextual Constructor Shorthand

A constructor expression written as `.Constructor` (with a leading dot and no
type prefix) is resolved using the expected type at the point of use. The
compiler uses the context to determine which type the constructor belongs to.

### In a return statement

The declared return type provides the expected type:

```solidity
data TxStatus = Pending | Confirmed | Reverted;

function initialStatus() -> TxStatus {
    return .Pending;    // resolved as TxStatus.Pending from the return type
}
```

### In a typed assignment

The declared type of the left-hand side provides the expected type:

```solidity
data TxStatus = Pending | Confirmed | Reverted;

function demo() -> TxStatus {
    let s : TxStatus;
    s = .Confirmed;     // resolved as TxStatus.Confirmed from the declared type of s
    return s;
}
```

### Error: ambiguous shorthand

If no expected type is available, the shorthand cannot be resolved and the
compiler reports an error:

```solidity
data TxStatus = Pending | Confirmed | Reverted;

function bad() -> word {
    let x = .Pending;   // no expected type available for x
    return 0;
}
```

```
Cannot resolve shorthand constructor expression without expected constructor type:
.Pending
```

The fix is to annotate the variable: `let x : TxStatus = .Pending;`.

---

## Integer Literals

An integer literal always has type `word`. There is no numeric type class or
overloading for integer literals in SAIL. Every integer literal that appears
in source code is a 256-bit EVM word value.

```solidity
function demo() -> word {
    let amount   = 1000;
    let decimals = 18;
    let mask     = 0xffffffffffffffffffffffffffffffffffffffff;
    return amount;     // amount, decimals, mask all have type word
}
```

---

## Polymorphic Function Call Inference

When a polymorphic function is called, the compiler instantiates the type
variables from the types of the supplied arguments. No explicit type
application is needed.

```solidity
forall a . function id(x : a) -> a {
    return x;
}

function demo() -> word {
    return id(42);    // a instantiated to word at this call site
}
```

For a pair function with two type variables, both are instantiated
independently:

```solidity
data Pair(a, b) = Pair(a, b);

forall a b . function fst(p : Pair(a, b)) -> a {
    match p {
    | Pair(x, y) => return x;
    }
}

function demo() -> word {
    return fst(Pair(42, true));    // a = word, b = bool
}
```

---

## Constraint Inference

When a function body calls a type class method, the compiler generates a
constraint on the type variable involved. If the function is top-level and
already carries an annotation, the annotation must list the constraint
explicitly. If the annotation omits the constraint, the subsumption check
catches the mismatch.

At a call site where the type variable is fixed to a concrete type, the
compiler checks that an instance exists for that type. If no instance is
found, the compiler reports an unsolved constraint error.

### Constraint resolved at call site

```solidity
forall a . class a:Encodable {
    function encode(x : a) -> word;
}

instance word:Encodable {
    function encode(x : word) -> word { return x; }
}

forall a . a:Encodable => function encodeField(x : a) -> word {
    return Encodable.encode(x);
}

contract ERC20 {
    function main() -> word {
        return encodeField(42);    // a = word; word:Encodable resolved
    }
}
```

### Error: no instance for the required type

If the concrete type at the call site has no matching instance, the compiler
reports which constraint could not be satisfied and which instances are
defined:

```solidity
forall a . class a:SafeArith {
    function safeAdd(x : a, y : a) -> a;
}

// No instance for word is declared.
function bad(x : word, y : word) -> word {
    return SafeArith.safeAdd(x, y);
}
```

```
Cannot entail:
word : SafeArith
using defined instances:

```

The fix is either to declare `instance word:SafeArith { ... }` or to add the
constraint to the calling function's signature so the obligation is propagated
to the caller:

```solidity
forall a . a:SafeArith => function bad(x : a, y : a) -> a {
    return SafeArith.safeAdd(x, y);
}
```

---

## Phantom Type Variables

A _phantom type parameter_ is a type parameter of a data type that does not
appear in any constructor field. When a phantom constructor is used inside a
function body and the phantom parameter cannot be determined from the context,
the compiler reports an ambiguous type variable error.

```solidity
data TypedSlot(a) = TypedSlot(word);    // a is phantom: it appears in no field

function bad() -> word {
    let s = TypedSlot.TypedSlot(0);     // a is unconstrained; no context fixes it
    return 0;
}
```

```
Ambiguous type variable(s) $1 in definition of bad.
This typically occurs when a constructor has phantom type parameters.
Please, add a type signature to fix the ambiguous type variable.
```

The fix is to annotate the `let` declaration with the full type, giving the
phantom parameter a concrete value:

```solidity
function good() -> word {
    let s : TypedSlot(word) = TypedSlot.TypedSlot(0);
    return 0;
}
```

---

## Unification Errors

A unification error occurs when two types that must be equal turn out to be
incompatible. The compiler reports the two types and the expression that
triggered the failure.

### Return type mismatch

```solidity
function bad(amount : word) -> bool {
    return amount;    // amount : word; expected bool
}
```

```
Types: bool and word do not unify
 - in: function bad (amount : word) -> bool { return amount; }
```

### Match arm return type mismatch

All arms of a `match` expression must produce the same type. Returning
different types in different arms is a unification error:

```solidity
data Result = Ok(word) | Err(word);

function bad(r : Result) -> word {
    match r {
    | Result.Ok(v)  => return v;
    | Result.Err(_) => return false;    // word expected; bool returned
    }
}
```

```
Types: bool and word do not unify
 - in: false
 - in: function bad (r : Result) -> word { ... }
```

### Algebraic data type vs primitive mismatch

User-defined types and primitive types such as `word` are never
interchangeable:

```solidity
data TxStatus = Pending | Confirmed;

function bad(n : word) -> TxStatus {
    return n;    // word is not TxStatus
}
```

```
Types: TxStatus and word do not unify
 - in: function bad (n : word) -> TxStatus { return n; }
```

---

## Scope of Inference

Inference is local to function bodies. Each function is checked independently,
and no type information flows between sibling functions except through their
declared signatures. The declared signature of a function is the only
interface that callers see; the body is invisible to inference in other
functions.

This means that the order of function definitions in a file does not affect
the types that inference assigns to expressions inside any given function.
Mutually recursive functions are type-checked as a group (see
[Functions](functions.md)), but even then each function's signature must carry
complete annotations.
