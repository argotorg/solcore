# Parametric Polymorphism

A parametric polymorphic function works uniformly over any type. The caller
does not need to know which concrete type is used; the function behaves
identically for all instantiations. SAIL supports parametric polymorphism
through `forall` quantifiers in function signatures.

---

## Type Variables and `forall`

A type variable is a placeholder for any concrete type. To introduce type
variables in a function signature, place a `forall` quantifier before the
`function` keyword. The quantifier lists the type variable names separated by
spaces and terminated by a period.

```solidity
forall a . function id(x : a) -> a {
    return x;
}
```

The signature `forall a . a -> a` states that `id` accepts one argument of any
type `a` and returns a value of the same type `a`. The same type variable `a`
appears in both the parameter and the return position, so the caller knows that
the output type equals the input type.

Multiple type variables are listed in the same quantifier:

```solidity
forall a b . function fst(p : (a, b)) -> a {
    match p {
    | (x, y) => return x;
    }
}

forall a b . function snd(p : (a, b)) -> b {
    match p {
    | (x, y) => return y;
    }
}
```

Type variables may appear in parameter types, the return type, and in
type arguments to other type constructors:

```solidity
data Option(a) = None | Some(a);

forall a . function just(x : a) -> Option(a) {
    return Option.Some(x);
}

forall a . function fromOption(default : a, opt : Option(a)) -> a {
    match opt {
    | Option.None    => return default;
    | Option.Some(x) => return x;
    }
}
```

---

## Call-Site Instantiation

At each call site, the compiler determines the concrete type for every type
variable from the types of the supplied arguments. No explicit type application
is needed; the inference engine handles instantiation automatically.

```solidity
forall a . function id(x : a) -> a {
    return x;
}

contract C {
    function main() -> word {
        return id(42);      // a is instantiated to word
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
data Pair(a, b) = Pair(a, b);

forall a b . function fst(p : Pair(a, b)) -> a {
    match p {
    | Pair(x, y) => return x;
    }
}

forall a b . function snd(p : Pair(a, b)) -> b {
    match p {
    | Pair(x, y) => return y;
    }
}

function addAmounts(x : word, y : word) -> word {
    let res : word;
    assembly { res := add(x, y) }
    return res;
}

// A transfer record holds (sender address, amount).
function totalTransferred(p : Pair(word, word)) -> word {
    return addAmounts(fst(p), snd(p));
}

contract ERC20 {
    function main() -> word {
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
data Option(a) = None | Some(a);

forall a . function orElse(primary : Option(a), fallback : Option(a)) -> Option(a) {
    match primary {
    | Option.Some(v) => return Option.Some(v);
    | Option.None    => return pickFirst(fallback, primary);
    }
}

forall a . function pickFirst(x : Option(a), y : Option(a)) -> Option(a) {
    match x {
    | Option.Some(v) => return orElse(x, y);
    | Option.None    => return y;
    }
}
```

---

## The Subsumption Test

When a function carries a `forall` annotation, the compiler verifies that the
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
forall a . function wrong(x : word) -> a {
    return x;
}
```

```
Type not polymorphic enough! The annotated type is:
forall a . word -> a
but the infered type is:
word -> word
in:
forall a . function wrong (x : word) -> a
```

The body `return x` has type `word -> word` because `x` is declared as
`word`. The skolemised declared type requires the result to be a rigid
variable `a`, which cannot be unified with `word`. The compiler rejects the
definition.

### Error: wrong type variable in the return position

A function that swaps the return type variable is caught by the same test.

```solidity
// Error: the body returns the first component (type a),
//        but the annotation declares the return type as b.
forall a b . function fst(p : (a, b)) -> b {
    match p {
    | (x, y) => return x;
    }
}
```

```
Type not polymorphic enough! The annotated type is:
forall a b . (a, b) -> b
but the infered type is:
forall $t . ($t, $t) -> $t
in:
forall a b . function fst (p : (a, b)) -> b
```

The body returns `x`, which has the type of the first component. The inferred
type therefore unifies both components and the return, making them all the
same variable `$t`. The skolemised declared type requires the return to be the
rigid variable `b` (the second component), which is distinct from `a`. The
unification fails and the compiler reports the error.

### Error: type variable forced to `word` by an assembly block

Assembly blocks operate exclusively on `word` values. If the body uses a type
variable as if it were `word` inside an assembly block, the inference engine
forces that variable to `word`, making the function monomorphic in the body
while the annotation still declares a type variable.

```solidity
// Error: the assembly block forces a to word,
//        so the body is monomorphic.
forall a . function double(x : a) -> a {
    let res : word;
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
forall a . function double (x : a) -> a
```

The correct way to write this function is to restrict the parameter type to
`word` explicitly and drop the `forall`:

```solidity
function double(x : word) -> word {
    let res : word;
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
