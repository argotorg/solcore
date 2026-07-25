# Type Classes

A type class defines a named set of operations that a type must implement. Any
type that provides implementations for all required operations is said to be an
_instance_ of the class. Type classes enable constrained polymorphism: a
function may be parameterized over a type variable and simultaneously require
that the variable belongs to one or more classes.

---

## Trait Declarations

A trait declaration introduces a type-class name, a main type variable, and
zero or more method signatures. Generic parameters follow the trait name in
angle brackets.

```solidity
trait Eq<A> {
    function eq(x: A, y: A) returns (bool);
    function ne(x: A, y: A) returns (bool);
}
```

The first type variable, `A`, is the _main type argument_ of the type class.
Every implementation must supply a concrete type for this variable.

A class with no methods defines a pure marker class:

```solidity
trait Serializable<A> {}
```

### Superclass Constraints

A trait may require that its main type argument already belongs to another type
class. This constraint is called a _superclass constraint_ and follows the
trait head in a `where` clause.

```solidity
trait Ord<A> where A: Eq {
    function lt(x: A, y: A) returns (bool);
    function lte(x: A, y: A) returns (bool);
}
```

Any implementation of `Ord` must also satisfy `Eq`. The compiler verifies this
at each impl declaration. If a function requires `A: Ord`, the constraint
`A: Eq` is automatically available without listing it explicitly.

---

## Impl Declarations

An impl declaration provides implementations for all methods of a trait for a
specific type. The impl head names the trait and supplies a concrete type for
the main type variable.

```solidity
impl Eq<word> {
    function eq(x: word, y: word) returns (bool) {
        let res: word;
        assembly { res := eq(x, y) }
        return res;
    }
    function ne(x: word, y: word) returns (bool) {
        let res: word;
        assembly { res := iszero(eq(x, y)) }
        return res;
    }
}
```

A polymorphic implementation applies to a family of types. Generic parameters
on `impl` list the type variables that appear in the impl head:

```solidity
enum Pair<A, B> {
    Pair(A, B)
}

impl<A, B> Eq<Pair<A, B>> where A: Eq, B: Eq {
    function eq(x: Pair<A, B>, y: Pair<A, B>) returns (bool) {
        match (x, y) {
            case (Pair(xa, xb), Pair(ya, yb)) {
                return Eq.eq(xa, ya);
            }
        }
    }
    function ne(x: Pair<A, B>, y: Pair<A, B>) returns (bool) {
        return Eq.ne(x, y);
    }
}
```

### Calling Class Methods

Trait methods are called with a qualified name of the form `TraitName.method`.
The compiler resolves the correct implementation from the argument types:

```solidity
enum Option<A> {
    None,
    Some(A)
}

function senderMatches<A>(sender: A, expected: Option<A>)
    returns (bool)
    where A: Eq
{
    match (expected) {
        case Option.None {
            return false;
        }
        case Option.Some(e) {
            return Eq.eq(sender, e);
        }
    }
}
```

### Overlapping Implementations

SAIL does not support overlapping implementations. Two impls overlap when the
same type can match both heads. The compiler reports an error at the second
declaration:

```solidity
enum Box<A> {
    Box(word)
}
trait C<A> {}

impl<A> C<Box<A>> {}

// Error: overlaps with the more general implementation above.
impl C<Box<word>> {}
```

```
Overlapping implementations are not supported
impl C<Box<word>>
overlaps with:
impl<T> C<Box<T>>
```

---

## Main and Weak Type Arguments

When a trait has more than one type parameter, the first parameter is called
the _main type argument_. The remaining parameters are called _weak type
arguments_.

```solidity
//          main ──┐         ┌── weak
trait Convert<A, B> {
    function convert(x: A) returns (B);
}
```

The distinction matters for instance resolution and for the three soundness
conditions the compiler enforces.

**Main type argument** (`A` in `Convert<A, B>`): used as the primary key for
instance lookup. The compiler selects an instance by matching the main type
first. It must be determinable independently of the weak arguments.

**Weak type arguments** (`B` in `Convert<A, B>`): represent additional types
involved in the relationship. They may be determined by the main type argument
through the coverage condition, but they cannot introduce type variables that
are unconstrained at the call site.

### Example: weak argument determined by main type

In the following instance, the main type `Wei` uniquely determines the weak
type `Ether`. The instance is well formed because the weak type variable is
replaced by a concrete type:

```solidity
trait Convert<A, B> {
    function convert(x: A) returns (B);
}

enum Wei {
    Wei(word)
}
enum Ether {
    Ether(word)
}

impl Convert<Wei, Ether> {
    function convert(x: Wei) returns (Ether) {
        match (x) {
            case Wei.Wei(w) {
                let e: word;
                assembly { e := div(w, 1000000000000000000) }
                return Ether.Ether(e);
            }
        }
    }
}

contract C {
    function main() returns (word) {
        let result = Convert.convert(Wei.Wei(2000000000000000000));
        match (result) {
            case Ether.Ether(v) {
                return v;
            }
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
enum Box<A> {
    Box(word)
}
trait MyClass<A, B> {}

// Error: B appears only in the weak position; Box<A> does not determine B.
impl<A, B> MyClass<Box<A>, B> {}
```

```
Coverage condition fails for class:
MyClass
- the type:
Box<A>
does not determine:
B
```

**Accepted example**

Replacing the unconstrained variable `B` with a concrete type eliminates the
violation:

```solidity
impl<A> MyClass<Box<A>, word> {}
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
trait C1<A> {}
trait C2<A> {}

// Context: U:C1 has measure 2, U:C2 has measure 2, total 4.
// Head:    U:C1 has measure 2.
// Context measure (4) is not strictly smaller than head measure (2).
impl<U> C1<U> where U: C1, U: C2 {}
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
enum Wrap<A> {
    Wrap(A)
}

// Context: U:C1 has measure 2.
// Head:    Wrap<U>: C1 has measure 3 (Wrap + U + C1 name).
// 2 < 3, so the Patterson condition holds.
impl<U> C1<Wrap<U>> where U: C1 {}
```

### Bound Variable Condition

Every type variable that appears in the instance context must also appear in the
instance head. A type variable present only in the context cannot be determined
from the types at the call site, making instance resolution ambiguous.

**Rejected example**

```solidity
enum Box<A> {
    Box(word)
}
trait Eq<A> {}
trait Container<A, B> {}

// Error: C appears in the context constraint C: Eq
//        but not in the impl head Container<Box<A>, A>.
impl<A, C> Container<Box<A>, A> where C: Eq {}
```

```
Bounded variable condition fails!
```

**Accepted example**

Remove the unused variable from the context, or include it in the head:

```solidity
// No context needed.
impl<A> Container<Box<A>, A> {}

// Or: bring C into the head through the weak argument.
impl<A, C> Container<Box<A>, C> where C: Eq {}
```

---

## Pragmas

A pragma is a compiler directive that relaxes one of the three instance
soundness conditions. Pragmas are written at the top level of a source file,
before any declarations.

There are three pragmas, one per condition:

| Pragma keyword                  | Condition disabled       |
| ------------------------------- | ------------------------ |
| `pragma solcore noCoverageCondition`      | Coverage condition       |
| `pragma solcore noPattersonCondition`     | Patterson condition      |
| `pragma solcore noBoundVariableCondition` | Bound variable condition |

Each pragma has two forms:

```solidity
// Disable for a specific list of classes (comma-separated).
pragma solcore noCoverageCondition ClassName1, ClassName2;

// Disable globally for all classes in this file.
pragma solcore noCoverageCondition;
```

Pragmas apply only to the file in which they appear. Importing a file does not
inherit its pragmas, and the importing file's pragmas do not affect the imported
declarations.

> **Warning** Disabling these conditions can allow instances that cause the
> compiler's instance resolution to loop or produce incoherent results. Use
> pragmas only when you understand the implications for the specific class and
> instance involved.

### `pragma solcore noCoverageCondition`

Disables the coverage check for the listed classes. Use this when a weak type
argument is deliberately left undetermined by the main type, for example in open
type-indexed families where the relationship is established by context rather
than by the instance itself.

```solidity
pragma solcore noCoverageCondition MyClass;

enum Box<A> {
    Box(word)
}
trait MyClass<A, B> {}

// Accepted: coverage condition is disabled for MyClass.
impl<A, B> MyClass<Box<A>, B> {}
```

Without the pragma, this declaration would produce:

```
Coverage condition fails for class:
MyClass
- the type:
Box<A>
does not determine:
B
```

### `pragma solcore noPattersonCondition`

Disables the Patterson measure check for the listed classes. Use this for class
hierarchies where the instance search is known to terminate through structural
arguments not captured by the simple measure metric.

```solidity
pragma solcore noPattersonCondition C1;

trait C1<A> {}
trait C2<A> {}

// Accepted: Patterson condition is disabled for C1.
impl<U> C1<U> where U: C1, U: C2 {}
```

Without the pragma, this declaration would produce:

```
Instance
U : C1
does not satisfy the Patterson conditions.
```

### `pragma solcore noBoundVariableCondition`

Disables the bound variable check for the listed classes. Use this when a
context variable is intentionally existential, meaning it is chosen by the
instance rather than derived from the call site.

```solidity
pragma solcore noBoundVariableCondition Container;

enum Box<A> {
    Box(word)
}
trait Eq<A> {}
trait Container<A, B> {}

// Accepted: bound variable condition is disabled for Container.
impl<A, C> Container<Box<A>, A> where C: Eq {}
```

Without the pragma, this declaration would produce:

```
Bounded variable condition fails!
```

### Combining Pragmas

Multiple pragmas may appear in the same file and may target the same class from
different directives. All specified conditions are disabled independently:

```solidity
pragma solcore noCoverageCondition MyClass;
pragma solcore noPattersonCondition MyClass;
pragma solcore noBoundVariableCondition MyClass;

enum Box<A> {
    Box(word)
}
trait Eq<A> {}
trait C1<A> {}
trait MyClass<A, B> {}

// Accepted: all three conditions are disabled for MyClass.
impl<A, B, C> MyClass<Box<A>, B> where C: Eq, (A, B): C1 {}
```
