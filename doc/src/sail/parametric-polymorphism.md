# Parametric Polymorphism

A parametric polymorphic function works uniformly over any type. The caller
does not need to know which concrete type is used; the function behaves
identically for all instantiations. SAIL supports parametric polymorphism
through generic parameter lists in function signatures.

---

## Type Variables and Generic Parameters

A type variable is a placeholder for any concrete type. To introduce type
variables in a function signature, list their names in angle brackets after the
function name.

```solidity
function id<A>(x: A) returns (A) {
    return x;
}
```

The signature `function id<A>(x: A) returns (A)` states that `id` accepts one
argument of any type `A` and returns a value of the same type `A`. The same
type variable `A`
appears in both the parameter and the return position, so the caller knows that
the output type equals the input type.

Multiple type variables are separated by commas:

```solidity
function fst<A, B>(p: (A, B)) returns (A) {
    match (p) {
        case (x, y) {
            return x;
        }
    }
}

function snd<A, B>(p: (A, B)) returns (B) {
    match (p) {
        case (x, y) {
            return y;
        }
    }
}
```

Type variables may appear in parameter types, the return type, and in
type arguments to other type constructors:

```solidity
enum Option<A> {
    None,
    Some(A)
}

function just<A>(x: A) returns (Option<A>) {
    return Option.Some(x);
}

function fromOption<A>(defaultValue: A, opt: Option<A>) returns (A) {
    match (opt) {
        case Option.None {
            return defaultValue;
        }
        case Option.Some(x) {
            return x;
        }
    }
}
```

---

## Call-Site Instantiation

At each call site, the compiler determines the concrete type for every type
variable from the types of the supplied arguments. No explicit type application
is needed; the inference engine handles instantiation automatically.

```solidity
function id<A>(x: A) returns (A) {
    return x;
}

contract C {
    function main() returns (word) {
        return id(42);      // A is instantiated to word
    }
}
```

Each combination of concrete types produces a distinct specialization during
compilation. A call to `id` with a `word` argument becomes `id$word` in the
generated output, and a call with a pair type becomes a separate function with
a distinct name. No polymorphism survives to the generated Yul.

---

## Polymorphic Functions with Pattern Matching

Polymorphic functions frequently deconstruct structured values through pattern
matching. The match compiler operates on the inferred type at each call site
after instantiation.

```solidity
enum Pair<A, B> {
    Pair(A, B)
}

function fst<A, B>(p: Pair<A, B>) returns (A) {
    match (p) {
        case Pair(x, y) {
            return x;
        }
    }
}

function snd<A, B>(p: Pair<A, B>) returns (B) {
    match (p) {
        case Pair(x, y) {
            return y;
        }
    }
}

function addAmounts(x: word, y: word) returns (word) {
    let res: word;
    assembly { res := add(x, y) }
    return res;
}

// A transfer record holds (sender address, amount).
function totalTransferred(p: Pair<word, word>) returns (word) {
    return addAmounts(fst(p), snd(p));
}

contract ERC20 {
    function main() returns (word) {
        return totalTransferred(Pair(100, 200));
    }
}
```

---

## Mutually Recursive Polymorphic Functions

Polymorphic functions may call each other recursively. The compiler resolves
mutual dependencies through strongly-connected-component analysis and checks
all functions in a group together. Both functions must be defined in the same
file.

```solidity
enum Option<A> {
    None,
    Some(A)
}

function orElse<A>(primary: Option<A>, fallbackValue: Option<A>)
    returns (Option<A>)
{
    match (primary) {
        case Option.Some(v) {
            return Option.Some(v);
        }
        case Option.None {
            return pickFirst(fallbackValue, primary);
        }
    }
}

function pickFirst<A>(x: Option<A>, y: Option<A>) returns (Option<A>) {
    match (x) {
        case Option.Some(v) {
            return orElse(x, y);
        }
        case Option.None {
            return y;
        }
    }
}
```

---

## The Subsumption Test

When a function declares generic parameters, the compiler verifies that the
body is at least as polymorphic as the declared signature. This check is called
the _subsumption test_. It prevents signatures that claim more generality than
the body actually provides.

The test works in three steps:

1. The declared type is _skolemised_: each type variable is replaced by a
   fresh rigid constant that cannot be unified with any other type.
2. The body is type-checked independently, producing an inferred type.
3. The inferred type must unify with the skolemised declared type. If a rigid
   constant would need to be unified with a concrete type (such as `word`),
   the body is not polymorphic enough and the compiler reports an error.

### Error: return type is more polymorphic than the body

The most common subsumption failure occurs when the annotation promises that
the function works for any type `a`, but the body always produces a specific
type such as `word`.

```solidity
// Error: the body always returns word, but the annotation says a.
function wrong<A>(x: word) returns (A) {
    return x;
}
```

```
Type not polymorphic enough! The annotated type is:
forall a . word -> a
but the infered type is:
word -> word
in:
function wrong<A>(x: word) returns (A)
```

The diagnostic renders inferred type schemes with mathematical `forall` and
`->` notation; those tokens are not source syntax. The body `return x` has
scheme `word -> word` because `x` is declared as `word`. The skolemised declared
type requires the result to be a rigid variable `A`, which cannot be unified
with `word`. The compiler rejects the
definition.

### Error: wrong type variable in the return position

A function that swaps the return type variable is caught by the same test.

```solidity
// Error: the body returns the first component (type A),
//        but the annotation declares the return type as B.
function fst<A, B>(p: (A, B)) returns (B) {
    match (p) {
        case (x, y) {
            return x;
        }
    }
}
```

```
Type not polymorphic enough! The annotated type is:
forall a b . (a, b) -> b
but the infered type is:
forall $t . ($t, $t) -> $t
in:
function fst<A, B>(p: (A, B)) returns (B)
```

The body returns `x`, which has the type of the first component. The inferred
type therefore unifies both components and the return, making them all the
same variable `$t`. The skolemised declared type requires the return to be the
rigid variable `B` (the second component), which is distinct from `A`. The
unification fails and the compiler reports the error.

### Error: type variable forced to `word` by an assembly block

Assembly blocks operate exclusively on `word` values. If the body uses a type
variable as if it were `word` inside an assembly block, the inference engine
forces that variable to `word`, making the function monomorphic in the body
while the annotation still declares a type variable.

```solidity
// Error: the assembly block forces A to word,
//        so the body is monomorphic.
function double<A>(x: A) returns (A) {
    let res: word;
    assembly { res := add(x, x) }
    return res;
}
```

```
Type not polymorphic enough! The annotated type is:
forall a . a -> a
but the infered type is:
word -> word
in:
function double<A>(x: A) returns (A)
```

The correct way to write this function is to restrict the parameter type to
`word` explicitly and drop the generic parameter:

```solidity
function double(x: word) returns (word) {
    let res: word;
    assembly { res := add(x, x) }
    return res;
}
```

If a computation must be polymorphic in a type class sense (working for all
types that support addition), use a constrained type variable instead of an
assembly block. See the [Type Classes](typeclasses.md) section for details.

---

## Specialization and Naming

The compiler eliminates all polymorphism before code generation through a
process called _specialization_ (or monomorphization). Every call site that
instantiates a polymorphic function at a concrete type combination produces a
separate function definition in the output. The compiler chooses names of the
form `name$Type` for each specialization, for example `id$word` or
`fst$word$bool`.

This means:

- There is no runtime representation of type variables.
- Each specialized version is compiled independently and can be optimized
  on its own.
- Whole-program compilation is required: the specializer must see all call
  sites to determine which instantiations to generate.

> **Note** A polymorphic function that is never called is not emitted at all.
> Only the specializations that are actually needed by the program appear in
> the compiled output.
