# Functions

Functions can be defined at the top level of a source file (called _free
functions_) or inside a contract body. Free functions are shared across all
contracts that call them; contract functions have access to the contract's
field variables.

```solcore
// A free function, visible to any contract that imports this file.
function double(x : word) -> word {
    let res : word;
    assembly { res := add(x, x) }
    return res;
}

contract Example {
    // A contract function with access to field 'value'.
    value : word;

    function store(v : word) {
        value = v;
    }
}
```

---

## Function Parameters

Function parameters are declared as a comma-separated list inside parentheses.
Each parameter may carry an explicit type annotation of the form `name : Type`
or may omit the annotation, in which case the compiler infers the type.

### Typed Parameters

A typed parameter fixes the parameter's type precisely. The compiler verifies
that every call site supplies an argument of the declared type.

```solcore
function add(x : word, y : word) -> word {
    let res : word;
    assembly { res := add(x, y) }
    return res;
}
```

### Untyped Parameters

When a parameter carries no type annotation, the compiler assigns it a fresh
type variable and determines its type from the function body and the call
sites.

```solcore
// x and y are untyped; the compiler infers their types.
function const(x, y) {
    return x;
}
```

Calling `const(42, false)` causes the compiler to infer `x : word` and
`y : bool`. If the function is used at multiple incompatible types across
different call sites, a type error is reported.

> **Note** Having at least one typed parameter or an explicit return type
> activates _checking mode_ for the function. In checking mode the compiler
> verifies the body against the declared types and then performs a subsumption
> check between the inferred scheme and the declared scheme. With no
> annotations at all the compiler runs in _inference mode_ and generalizes
> the inferred type automatically.

---

## Return Type

The return type is declared after the parameter list with the `->` arrow.

```solcore
function identity(x : word) -> word {
    return x;
}
```

The return type may be omitted. When omitted, the compiler infers it from the
`return` statements in the body. All `return` statements must produce values
of the same type.

```solcore
// Return type is inferred as 'word'.
function increment(x : word) {
    return add(x, 1);
}
```

> **Note** A function with no `return` statement implicitly returns `()` (the
> unit value). Functions that execute assembly blocks for their side effects
> and do not return a meaningful value should be declared with return type
> `->  ()` or omit the return type entirely.

---

## Type Inference

SAIL implements constraint-based bidirectional type inference. The compiler
propagates type information both bottom-up (from expressions to their
containing statements) and top-down (from expected types into subexpressions).

### Inference Mode (Unannotated Functions)

When a function has no type annotations, the compiler:

1. Assigns a fresh type variable to each parameter and to the function itself.
2. Type-checks the body, collecting equality constraints between type
   variables.
3. Solves the constraints by unification.
4. Generalizes the resulting type into a polymorphic scheme by universally
   quantifying over all type variables that are not fixed by the environment.

```solcore
// The compiler infers: forall a b . function const(x : a, y : b) -> a
function const(x, y) {
    return x;
}
```

### Checking Mode (Annotated Functions)

When at least one parameter is typed or the return type is given, the
compiler:

1. Skolemizes the declared type — it replaces universally quantified type
   variables with rigid _skolem constants_ that cannot be unified with
   anything other than themselves.
2. Checks the body against the expected return type.
3. Infers the function's type scheme from the checked body.
4. Performs a _subsumption check_: the inferred scheme must be at least as
   polymorphic as the declared scheme. If the inferred type is less general,
   the compiler reports a type error.

```solcore
// Declared return type 'word' constrains the body.
function toWord(x : word) -> word {
    return x;
}
```

If the declared type is more polymorphic than what the body supports, a type
error is produced:

```solcore
// Error: the body always returns x of type word,
//        so the declared type 'forall a. word -> a' cannot be satisfied.
forall a . function fromWord(x : word) -> a {
    return x;   // x : word, but the declared return type is 'a'
}
```

### Type Annotations on Expressions

Anywhere in an expression a type annotation `expr : Type` can be written to
guide inference. This is useful to disambiguate constructor applications whose
return type is not determined by the arguments.

```solcore
data Proxy(a) = Proxy;

function proxyWord() -> Proxy(word) {
    // Without the annotation the compiler cannot determine 'a'.
    return Proxy : Proxy(word);
}
```

---

## Short-Form Function Bodies

When a function body consists of a single expression whose value is returned
immediately, the expression may be written directly inside the braces without
a `return` statement.

```solcore
// Long form
function double(x : word) -> word {
    return add(x, x);
}

// Short form — equivalent
function double(x : word) -> word { add(x, x) }
```

Short-form bodies are a syntactic convenience. The type checker treats them
identically to `{ return expr; }`.

---

## Polymorphic Functions

SAIL supports _parametric polymorphism_: a single function definition can work
uniformly over many types. A polymorphic function is introduced with a
`forall` quantifier that lists the type variables in scope for the signature.

```solcore
// Works for any type 'a'.
forall a . function identity(x : a) -> a {
    return x;
}

// Works for any two types 'a' and 'b'.
forall a b . function const(x : a, y : b) -> a {
    return x;
}
```

At every call site the type checker instantiates the type variables with
concrete types inferred from the arguments:

```solcore
contract Demo {
    function main() -> word {
        return identity(42);       // instantiated at a = word
    }
}
```

> **Note** Polymorphic functions are _monomorphized_ by the specializer
> before code generation. Each distinct instantiation produces a separate
> function in the output. The function `identity` called with a `word`
> argument becomes `identity$word` in the compiled output; called with a
> `bool` argument it becomes `identity$bool`. No polymorphism survives to
> the generated Yul.

---

## Type Class Constraints

A function can require that its type variables satisfy certain _type class
constraints_. Constraints are declared between the type variable list and the
`=>` arrow.

```solcore
forall t . class t:Add {
    function add(l : t, r : t) -> t;
}

instance word:Add {
    function add(l : word, r : word) -> word {
        let res : word;
        assembly { res := add(l, r) }
        return res;
    }
}

// Constraint: 't' must be an instance of 'Add'.
forall t . t:Add => function twice(x : t) -> t {
    return Add.add(x, x);
}
```

At each call site the compiler checks that the constraint is satisfied by
looking up the matching instance. If no instance exists for the types involved,
a type error is reported.

Multiple constraints are separated by commas:

```solcore
forall lhs rhs .
    lhs:IsWord, rhs:IsWord
    => function combine(l : lhs, r : rhs) -> word {
        return IsWord.toWord(l);
    }
```

Constraints can also involve multiple type parameters when a class is
parametrized:

```solcore
forall a b c . class a:Nth(b, c) {
    function nth(idx : Proxy(a), tup : b) -> c;
}

forall a b . instance Zero:Nth((a, b), a) {
    function nth(idx : Proxy(Zero), tup : (a, b)) -> a {
        match tup {
        | (x, _) => return x;
        }
    }
}
```

> **Note** Constraint solving follows the _Haskell class system_ rules. The
> compiler applies the Patterson conditions and coverage conditions to ensure
> instance resolution terminates. These conditions can be selectively relaxed
> with `pragma no-patterson-condition` or `pragma no-coverage-condition` when
> necessary.

---

## Recursive Functions

A function may call itself recursively. The compiler adds the function's own
name to the typing context before checking the body, so recursive calls are
handled naturally.

```solcore
data Nat = Zero | Succ(Nat);

function toWord(n : Nat) -> word {
    match n {
    | Nat.Zero    => return 0;
    | Nat.Succ(m) => return add(toWord(m), 1);
    }
}
```

Mutually recursive functions (where `f` calls `g` and `g` calls `f`) require
both functions to be in scope. The compiler resolves mutual recursion through
strongly-connected-component (SCC) analysis: all functions in the same SCC
are processed together with each other's types in scope.

---

## Free Functions vs. Contract Functions

A _free function_ is defined at the top level of a source file, outside any
contract. It is available to any contract or other free function that names it.

```solcore
// Free function: usable from any contract.
function add(x : word, y : word) -> word {
    let res : word;
    assembly { res := add(x, y) }
    return res;
}

contract Counter {
    count : word;

    // Contract function: has access to the 'count' field.
    function increment() {
        count = add(count, 1);
    }

    function main() -> word {
        increment();
        return count;
    }
}
```

Contract functions may read and write the contract's field variables. Free
functions cannot access any contract state directly; they operate only on
their parameters and locally declared variables.

---

## Pattern Matching in Function Bodies

Functions may use `match` statements to deconstruct algebraic data type
values. A `match` examines one or more scrutinees and selects the first arm
whose patterns match.

```solcore
data Option(a) = None | Some(a);

function maybe(default : word, opt : Option(word)) -> word {
    match opt {
    | Option.None    => return default;
    | Option.Some(x) => return x;
    }
}
```

Patterns may be nested arbitrarily:

```solcore
function join(mmx : Option(Option(word))) -> Option(word) {
    match mmx {
    | Option.Some(Option.Some(x)) => return Option.Some(x);
    | _                           => return Option.None;
    }
}
```

The wildcard pattern `_` matches any value without binding it. The compiler
checks that the set of patterns covers all possible values of the scrutinee
type. An incomplete match is a compile-time error.

---

## Assembly in Function Bodies

Functions may contain `assembly` blocks to access EVM opcodes directly. Inside
an assembly block, Yul syntax is used. Variables declared in the surrounding
SAIL scope are accessible by name inside the block.

```solcore
function add(x : word, y : word) -> word {
    let res : word;
    assembly {
        res := add(x, y)
    }
    return res;
}
```

Variables assigned inside an assembly block must have been declared with `let`
in the enclosing SAIL scope before the block. The type of such a variable must
be `word`, since Yul operates exclusively on 256-bit machine words.

> **Warning** The type checker cannot verify the semantic correctness of Yul
> code. Incorrect assembly can produce contracts that silently compute wrong
> results or revert unexpectedly. Minimize the size of assembly blocks and
> document any non-obvious invariants.
