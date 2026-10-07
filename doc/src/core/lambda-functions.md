# Lambda Functions

Core Solidity has anonymous functions, written with the `lam` keyword. A lambda
is an expression, so it can be bound to a variable, passed as an argument,
returned from a function, and applied inline.

```solidity
lam (x: word, y: word) -> word { return x + y; }
```

The parameter list is written in parentheses and the body in braces, as for a
named function. The result type is written after `->`; it may be omitted when
inference can recover it, and parameter types may likewise be left out where the
surrounding context fixes them:

```solidity
let increment = lam (x: word) -> word { return x + 1; };   // fully annotated
let identity  = lam (x) { return x; };                      // both inferred
let three     = (lam (x: word) -> word { return x + 2; })(1);  // applied inline
```

A function that returns nothing returns the unit type `()` (see
[Built-ins](../sail/builtins.md)); omitting the arrow result annotation on a
lambda whose body has no `return` value gives it that type.

## Closures

A lambda may refer to variables bound in the enclosing scope. Such a lambda is a
_closure_: it captures the values of those free variables at the point where it
is created.

```solidity
function adder(n: word) -> (word -> word) {
    return lam (x: word) -> word { return x + n; };   // captures n
}
```

The returned function carries its own copy of `n`. A lambda that refers to no
enclosing variable is _free_ and needs no captured environment.

## How lambdas are compiled

SAIL is a first-order language: it has no runtime function pointers and no
closures in the generated code. Lambdas are therefore eliminated **before type
checking**, by a defunctionalization pass (see the
[Compilation Pipeline](../compilation-pipeline.md)). For each lambda the compiler
generates:

1. a fresh data type whose single constructor holds the captured variables (an
   empty constructor for a free lambda),
2. a lifted top-level function containing the lambda body, taking the captured
   environment and the lambda's parameters, and
3. an instance of the primitive `invokable` trait for that data type, whose
   `invoke` method calls the lifted function.

A lambda expression then becomes a value of the generated type (the captured
environment), and a call `f(args)` becomes `invoke(f, args)` dispatched through
the trait. Because every function value has a concrete type and every call is a
trait method, the result type-checks and specializes like any other program, and
after specialization the indirection disappears entirely: a monomorphic call to a
known lambda is inlined to a direct call to its lifted function, so the deployed
code has no dynamic dispatch overhead.

This is the same `invokable`/`Method` machinery that the contract dispatcher uses
to treat a contract's public methods as callable values (see
[Contract Dispatch](../stdlib/dispatch.md)).

> **Note** Because defunctionalization runs before type checking, higher-order
> code must still be monomorphizable: function values whose captured shape or
> identity is only known at run time are outside what the first-order backend can
> express. Fully compile-time-resolved higher-order code works.
