# Datatypes

SAIL provides _algebraic data types_ (ADTs) for defining structured values.
An ADT declares a named type together with one or more _constructors_. Each
constructor describes one way to build a value of that type and may carry
zero or more _fields_ of arbitrary types.

```solcore
data Option(a) = None | Some(a);
```

Data types may be defined at the top level of a source file or inside a
contract body.

---

## Enumeration Types

The simplest kind of algebraic data type has only nullary constructors with no
fields. Such a type acts as a finite enumeration.

```solcore
data TokenStatus = Active | Paused | Deprecated;
```

Each constructor is a distinct value of the type. Enumerations are commonly
used wherever Solidity uses `enum`.

```solcore
contract Registry {
    data TokenStatus = Active | Paused | Deprecated;

    function statusCode(s : TokenStatus) -> word {
        match s {
        | TokenStatus.Active     => return 1;
        | TokenStatus.Paused     => return 2;
        | TokenStatus.Deprecated => return 0;
        }
    }

    function main() -> word {
        return statusCode(TokenStatus.Active);
    }
}
```

---

## Constructors with Fields

A constructor can carry one or more fields. The field types are listed in
parentheses, separated by commas.

```solcore
data TxStatus  = Pending | Settled | Failed;
data TxOutcome = Success(TxStatus) | Revert(TxStatus) | Unknown;
```

A constructor with fields is applied like a function: `TxOutcome.Success(TxStatus.Settled)`
produces a value of type `TxOutcome` wrapping a value of type `TxStatus`.

Fields are extracted by pattern matching; there is no record-style field
access. The pattern mirrors the constructor application:

```solcore
function outcomeCode(x : TxOutcome) -> word {
    match x {
    | TxOutcome.Success(TxStatus.Settled) => return 1;
    | TxOutcome.Revert(TxStatus.Failed)   => return 2;
    | _                                   => return 0;
    }
}
```

---

## Parametric Data Types

A data type can be parameterized by one or more _type variables_, making it
a _generic_ or _parametric_ type. The type variables are listed in parentheses
after the type name.

```solcore
data Option(a) = None | Some(a);
```

Here `a` is a type variable. `Option(word)` is the type of optional words,
`Option(bool)` is the type of optional booleans, and so on. The type variable
`a` may appear in the field types of any constructor.

```solcore
contract Option {
    data Option(a) = None | Some(a);

    function just(x : a) -> Option(a) {
        return Option.Some(x);
    }

    function maybe(default : word, opt : Option(word)) -> word {
        match opt {
        | Option.None    => return default;
        | Option.Some(x) => return x;
        }
    }

    function main() -> word {
        return maybe(0, Option.Some(42));
    }
}
```

The type checker verifies that every use of a parametric type supplies the
correct number of type arguments. Applying `Option` to two arguments, for
instance, is a type error.

---

## Nested Pattern Matching

Patterns may be nested to arbitrary depth to match inside multiple layers of
constructors in a single arm.

```solcore
data Option(a) = None | Some(a);

// Unwrap an approval amount nested in two Option layers.
function resolveApproval(outer : Option(Option(word))) -> Option(word) {
    match outer {
    | Option.Some(Option.Some(x)) => return Option.Some(x);
    | _                           => return Option.None;
    }
}
```

The wildcard `_` matches any value without binding it. It can appear at any
depth in a pattern.

---

## Opaque Wrappers

A single-constructor, single-field type is the standard idiom for introducing
a _distinct_ type that is represented by an existing type at runtime. This is
similar to Haskell's `newtype` or Solidity's user-defined value types.

```solcore
data uint256 = uint256(word);
```

`uint256` is a type distinct from `word` even though it carries exactly one
`word` field. The type checker treats them as incompatible, preventing
accidental mixing. The wrapper is removed during compilation: `uint256` values
occupy exactly one EVM word, just like `word`.

Wrapping and unwrapping are done explicitly with the constructor and a pattern:

```solcore
function wrap(x : word) -> uint256 {
    return uint256(x);
}

function unwrap(x : uint256) -> word {
    match x {
    | uint256(w) => return w;
    }
}
```

The standard library defines `uint256`, `uint8`, `uint16`, and `address` this
way, each wrapping `word`.

---

## Phantom Type Parameters

A type parameter that does not appear in any constructor field is called a
_phantom_ type parameter. It carries no runtime information but allows the
type system to distinguish values that would otherwise be identical.

```solcore
// 'a' is a phantom type parameter: the constructor Proxy carries no field of type 'a'.
data Proxy(a) = Proxy;
```

`Proxy(word)` and `Proxy(bool)` are distinct types at compile time but
produce the same runtime value. Phantom types are useful for passing type
information to functions without allocating extra memory.

```solcore
forall a . class a:MemoryType {
    function size(prx : Proxy(a)) -> word;
}

instance word:MemoryType {
    function size(prx : Proxy(word)) -> word {
        return 32;
    }
}
```

The `Proxy(a)` argument lets the caller select which `MemoryType` instance to
use without passing an actual value of type `a`.

> **Note** Because phantom type parameters leave the constructor's result
> type partially undetermined, the type checker requires an explicit type
> annotation whenever a `Proxy` value is constructed in a context where the
> type cannot be inferred from surrounding expressions. Use the expression
> annotation form `Proxy : Proxy(word)` to resolve the ambiguity.

---

## Tuples

SAIL has built-in support for _product types_ (tuples). A tuple type is written
as a parenthesised, comma-separated list of component types. Tuple values are
written the same way.

```solcore
function swap(p : (word, bool)) -> (bool, word) {
    match p {
    | (x, b) => return (b, x);
    }
}
```

Tuples of more than two elements are right-nested pairs internally. The type
`(word, bool, word)` is represented as `pair(word, pair(bool, word))`.

The unit type `()` is the zero-element tuple. It carries no information and
is used as the return type of functions that exist only for their side effects.

```solcore
function storeBalance(account : word, amount : word) -> () {
    assembly { sstore(account, amount) }
}
```

> **Note** Tuple patterns may appear anywhere a pattern is expected,
> including inside constructor patterns:
>
> ```solcore
> forall a b . instance Zero:Nth((a, b), a) {
>     function nth(idx : Proxy(Zero), tup : (a, b)) -> a {
>         match tup {
>         | (x, _) => return x;
>         }
>     }
> }
> ```

---

## Contextual Constructor Syntax

In a context where the expected type is known, the module qualifier can be
omitted from a constructor name by prefixing it with `.`. The compiler resolves
the constructor to the appropriate type automatically.

```solcore
data Option(a) = None | Some(a);

function just(x : word) -> Option(word) {
    return .Some(x);   // equivalent to Option.Some(x)
}

function nothing() -> Option(word) {
    return .None;      // equivalent to Option.None
}
```

The same shorthand works in patterns:

```solcore
function isNone(o : Option(word)) -> bool {
    match o {
    | .None    => return true;
    | .Some(_) => return false;
    }
}
```

The compiler reports an error if the expected type is not known or if the
constructor name is ambiguous.

---

## Type Synonyms

A _type synonym_ introduces a new name for an existing type. Synonyms are
purely a compile-time device: the compiler expands them before type checking
and they leave no trace in the generated code.

```solcore
type Int   = word;
type Point = pair(Int, Int);

function makePoint(x : Int, y : Int) -> Point {
    return (x, y);
}

function getX(p : Point) -> Int {
    match p {
    | (x, _) => return x;
    }
}
```

Like data types, synonyms can have type parameters:

```solcore
type Map(k, v) = pair(k, v);   // toy example
```

> **Warning** Recursive type synonyms are not allowed. A synonym must not
> refer directly or indirectly to itself. Attempting to define `type A = B`
> and `type B = A` simultaneously is a compile-time error.

---

## Runtime Encoding

Algebraic data types compile to a uniform binary encoding in the generated
Hull/Yul code.

**Sum types** (types with more than one constructor) are encoded as nested
binary sums using `inl` (left injection) and `inr` (right injection). A type
with _n_ constructors becomes a right-nested binary tree of depth ⌈log₂ n⌉.
For example, a three-constructor type `data T = A | B | C` is encoded as:

```
A  →  inl ()
B  →  inr (inl ())
C  →  inr (inr ())
```

**Product types** (constructor fields, tuples) are encoded as right-nested
pairs. The three-field constructor `data T = T(word, bool, word)` becomes
`pair(word, pair(bool, word))`.

This uniform encoding is what the `match` compiler and the Hull back-end
operate on. It is not visible at the SAIL level.
