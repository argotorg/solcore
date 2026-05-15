# Type Classes

A type class defines a named set of operations that a type must implement. Any
type that provides implementations for all required operations is said to be an
_instance_ of the class. Type classes enable constrained polymorphism: a
function may be parameterized over a type variable and simultaneously require
that the variable belongs to one or more classes.

---

## Class Declarations

A class declaration introduces a class name, a main type variable, and zero or
more method signatures. The `forall` quantifier is required when the class
declaration introduces type variables.

```solidity
forall a . class a:Eq {
    function eq(x : a, y : a) -> bool;
    function ne(x : a, y : a) -> bool;
}
```

The type variable `a` that appears immediately before the colon is the _main
type argument_ of the class. Every instance must supply a concrete type for this
variable.

A class with no methods defines a pure marker class:

```solidity
forall a . class a:Serializable {}
```

### Superclass Constraints

A class may require that its main type argument already belongs to another
class. This constraint is called a _superclass constraint_ and is written before
the `class` keyword with `=>`.

```solidity
forall a . a:Eq => class a:Ord {
    function lt(x : a, y : a) -> bool;
    function lte(x : a, y : a) -> bool;
}
```

Any instance of `Ord` must also be an instance of `Eq`. The compiler verifies
this at each instance declaration. If a function requires `a:Ord`, the
constraint `a:Eq` is automatically available without listing it explicitly.

---

## Instance Declarations

An instance declaration provides implementations for all methods of a class for
a specific type. The instance head names the class and supplies a concrete type
for the main type variable.

```solidity
instance word:Eq {
    function eq(x : word, y : word) -> bool {
        let res : word;
        assembly { res := eq(x, y) }
        return res;
    }
    function ne(x : word, y : word) -> bool {
        let res : word;
        assembly { res := iszero(eq(x, y)) }
        return res;
    }
}
```

A polymorphic instance applies to a family of types. The `forall` quantifier
lists the type variables that appear in the instance head:

```solidity
data Pair(a, b) = Pair(a, b);

forall a b . a:Eq, b:Eq => instance Pair(a, b):Eq {
    function eq(x : Pair(a, b), y : Pair(a, b)) -> bool {
        match x, y {
        | Pair(xa, xb), Pair(ya, yb) =>
            return Eq.eq(xa, ya);
        }
    }
    function ne(x : Pair(a, b), y : Pair(a, b)) -> bool {
        return Eq.ne(x, y);
    }
}
```

### Calling Class Methods

Class methods are called with a qualified name of the form `ClassName.method`.
The compiler resolves the correct instance from the types of the arguments:

```solidity
data Option(a) = None | Some(a);

forall a . a:Eq => function senderMatches(sender : a, expected : Option(a)) -> bool {
    match expected {
    | Option.None    => return false;
    | Option.Some(e) => return Eq.eq(sender, e);
    }
}
```

### Overlapping Instances

SAIL does not support overlapping instances. Two instances overlap when the same
type can match both instance heads. The compiler reports an error at the second
declaration:

```solidity
data Box(a) = Box(word);
forall a . class a:C {}

forall a . instance Box(a):C {}

// Error: overlaps with the more general instance above.
instance Box(word):C {}
```

```
Overlapping instances are not supported
instance:
Box(word) : C
overlaps with:
Box(?$3) : C
```

---

## Main and Weak Type Arguments

When a class has more than one type parameter, the parameter immediately before
the colon in the class head is called the _main type argument_. The remaining
parameters, listed after the class name in parentheses, are called _weak type
arguments_.

```solidity
//          main ──┐         ┌── weak
forall a b . class a:Convert(b) {
    function convert(x : a) -> b;
}
```

The distinction matters for instance resolution and for the three soundness
conditions the compiler enforces.

**Main type argument** (`a` in `a:Convert(b)`): used as the primary key for
instance lookup. The compiler selects an instance by matching the main type
first. It must be determinable independently of the weak arguments.

**Weak type arguments** (`b` in `a:Convert(b)`): represent additional types
involved in the relationship. They may be determined by the main type argument
through the coverage condition, but they cannot introduce type variables that
are unconstrained at the call site.

### Example: weak argument determined by main type

In the following instance, the main type `Wei` uniquely determines the weak
type `Ether`. The instance is well formed because the weak type variable is
replaced by a concrete type:

```solidity
forall a b . class a:Convert(b) {
    function convert(x : a) -> b;
}

data Wei   = Wei(word);
data Ether = Ether(word);

instance Wei:Convert(Ether) {
    function convert(x : Wei) -> Ether {
        match x {
        | Wei.Wei(w) =>
            let e : word;
            assembly { e := div(w, 1000000000000000000) }
            return Ether.Ether(e);
        }
    }
}

contract C {
    function main() -> word {
        let result = Convert.convert(Wei.Wei(2000000000000000000));
        match result {
        | Ether.Ether(v) => return v;
        }
    }
}
```

---

## Instance Soundness Conditions

To guarantee that instance resolution terminates and remains coherent, the
compiler enforces three conditions on every instance declaration. Violating any
of them is a compile-time error. Each condition can be relaxed by a pragma when
a specific instance is known to be safe.

### Coverage Condition

Every type variable that appears in a weak type argument position must be
determined by the main type argument. The set of type variables bound by the
main type must cover all type variables bound by the weak types.

**Rejected example**

```solidity
data Box(a) = Box(word);
forall a b . class a:MyClass(b) {}

// Error: b appears only in the weak position; Box(a) does not determine b.
forall a b . instance Box(a):MyClass(b) {}
```

```
Coverage condition fails for class:
MyClass
- the type:
Box(a)
does not determine:
b
```

**Accepted example**

Replacing the unconstrained variable `b` with a concrete type eliminates the
violation:

```solidity
forall a . instance Box(a):MyClass(word) {}
```

### Patterson Condition

For each constraint in the instance context, the _measure_ of the constraint
must be strictly smaller than the measure of the instance head. The measure of a
predicate is the total number of type constructors and type variables it
contains, counting repetitions. Each type constructor or type variable
contributes 1 to the measure, regardless of nesting.

This condition prevents instance search from entering an infinite loop when the
same type class is used in both the context and the head.

**Rejected example**

```solidity
forall a . class a:C1 {}
forall a . class a:C2 {}

// Context: U:C1 has measure 2, U:C2 has measure 2, total 4.
// Head:    U:C1 has measure 2.
// Context measure (4) is not strictly smaller than head measure (2).
forall U . U:C1, U:C2 => instance U:C1 {}
```

```
Instance
U : C1
does not satisfy the Patterson conditions.
```

**Accepted example**

Wrapping the main type in a constructor increases the head measure so that each
context constraint is strictly smaller:

```solidity
data Wrap(a) = Wrap(a);

// Context: U:C1 has measure 2.
// Head:    Wrap(U):C1 has measure 3 (Wrap + U + C1 name).
// 2 < 3, so the Patterson condition holds.
forall U . U:C1 => instance Wrap(U):C1 {}
```

### Bound Variable Condition

Every type variable that appears in the instance context must also appear in the
instance head. A type variable present only in the context cannot be determined
from the types at the call site, making instance resolution ambiguous.

**Rejected example**

```solidity
data Box(a) = Box(word);
forall a . class a:Eq {}
forall a b . class a:Container(b) {}

// Error: c appears in the context constraint c:Eq
//        but not in the instance head Box(a):Container(a).
forall a c . c:Eq => instance Box(a):Container(a) {}
```

```
Bounded variable condition fails!
```

**Accepted example**

Remove the unused variable from the context, or include it in the head:

```solidity
// No context needed.
forall a . instance Box(a):Container(a) {}

// Or: bring c into the head through the weak argument.
forall a c . c:Eq => instance Box(a):Container(c) {}
```

---

## Pragmas

A pragma is a compiler directive that relaxes one of the three instance
soundness conditions. Pragmas are written at the top level of a source file,
before any declarations.

There are three pragmas, one per condition:

| Pragma keyword                  | Condition disabled       |
| ------------------------------- | ------------------------ |
| `no-coverage-condition`         | Coverage condition       |
| `no-patterson-condition`        | Patterson condition      |
| `no-bounded-variable-condition` | Bound variable condition |

Each pragma has two forms:

```solidity
// Disable for a specific list of classes (comma-separated).
pragma no-coverage-condition ClassName1, ClassName2;

// Disable globally for all classes in this file.
pragma no-coverage-condition;
```

Pragmas apply only to the file in which they appear. Importing a file does not
inherit its pragmas, and the importing file's pragmas do not affect the imported
declarations.

> **Warning** Disabling these conditions can allow instances that cause the
> compiler's instance resolution to loop or produce incoherent results. Use
> pragmas only when you understand the implications for the specific class and
> instance involved.

### `pragma no-coverage-condition`

Disables the coverage check for the listed classes. Use this when a weak type
argument is deliberately left undetermined by the main type, for example in open
type-indexed families where the relationship is established by context rather
than by the instance itself.

```solidity
pragma no-coverage-condition MyClass;

data Box(a) = Box(word);
forall a b . class a:MyClass(b) {}

// Accepted: coverage condition is disabled for MyClass.
forall a b . instance Box(a):MyClass(b) {}
```

Without the pragma, this declaration would produce:

```
Coverage condition fails for class:
MyClass
- the type:
Box(a)
does not determine:
b
```

### `pragma no-patterson-condition`

Disables the Patterson measure check for the listed classes. Use this for class
hierarchies where the instance search is known to terminate through structural
arguments not captured by the simple measure metric.

```solidity
pragma no-patterson-condition C1;

forall a . class a:C1 {}
forall a . class a:C2 {}

// Accepted: Patterson condition is disabled for C1.
forall U . U:C1, U:C2 => instance U:C1 {}
```

Without the pragma, this declaration would produce:

```
Instance
U : C1
does not satisfy the Patterson conditions.
```

### `pragma no-bounded-variable-condition`

Disables the bound variable check for the listed classes. Use this when a
context variable is intentionally existential, meaning it is chosen by the
instance rather than derived from the call site.

```solidity
pragma no-bounded-variable-condition Container;

data Box(a) = Box(word);
forall a . class a:Eq {}
forall a b . class a:Container(b) {}

// Accepted: bound variable condition is disabled for Container.
forall a c . c:Eq => instance Box(a):Container(a) {}
```

Without the pragma, this declaration would produce:

```
Bounded variable condition fails!
```

### Combining Pragmas

Multiple pragmas may appear in the same file and may target the same class from
different directives. All specified conditions are disabled independently:

```solidity
pragma no-coverage-condition MyClass;
pragma no-patterson-condition MyClass;
pragma no-bounded-variable-condition MyClass;

data Box(a) = Box(word);
forall a . class a:Eq {}
forall a . class a:C1 {}
forall a b . class a:MyClass(b) {}

// Accepted: all three conditions are disabled for MyClass.
forall a b c . c:Eq, (a, b):C1 => instance Box(a):MyClass(b) {}
```
