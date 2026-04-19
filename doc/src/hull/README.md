# Hull

Hull is an intermediate language used by the Core Solidity compiler between the
typed frontend and the Yul backend. Every Core Solidity contract is lowered to
Hull after type-checking, monomorphization, and match compilation. Hull is then
translated to Yul for final code generation.

Hull retains algebraic data types — products and sums — from the source
language, but eliminates polymorphism, type classes, higher-order functions, and
all surface syntax sugar. The result is a first-order, monomorphic language
whose structure maps directly onto Yul constructs.

> **Note** Hull is produced by the compiler and is not intended to be written by
> hand. The concrete syntax described in this document exists to make diagnostic
> `.hull` output readable. It can be inspected by passing the `-dump-hull` flag
> to the `sol-core` binary.

---

## Relation to the Compilation Pipeline

![Hull compilation pipeline](diagrams/pipeline.svg)

The Core Solidity performs all stages up to and including Hull emission. The
`yule` compiler takes Hull as input and produces Yul. Yul is then compiled to
EVM bytecode by the standard `solc` compiler.

---

## Types

All types in Hull are monomorphic. Type variables are fully instantiated by the
specializer before Hull is produced.

### `word`

`word` is a 256-bit unsigned integer, corresponding to the native word size of
the EVM. All arithmetic values, storage addresses, and ABI-encoded data are
ultimately represented as `word`.

### `bool`

`bool` is the Boolean type. Its only values are the literals `true` and `false`.

### `unit`

`unit` is the zero-size type. Its only value is `()`. It carries no information
and occupies no storage. `unit` appears as the payload type of nullary
constructors in sum types. For example, the `None` branch of `Option(word)` has
payload type `unit`.

### Product Types

A product type is written as:

```
(T1 * T2)
```

It holds two values simultaneously, one of type `T1` and one of type `T2`. The
`*` operator is right-associative, so `A * B * C` is parsed as `A * (B * C)`.

The projections `fst` and `snd` extract the left and right components of a
product value, respectively.

> **Note** N-ary products are not yet implemented. Products with more than two
> components are encoded as right-nested pairs: `(A, B, C)` is represented as
> `(A * (B * C))`.

### Sum Types

A sum type is written as:

```
(T1 + T2)
```

It holds a value of either type `T1`, tagged as the left branch, or type `T2`,
tagged as the right branch. The `+` operator is right-associative, so
`A + B + C` is parsed as `A + (B + C)`.

Sum values are constructed using injection expressions (`inl`, `inr`, `in(k)`)
and deconstructed using `match` statements.

An n-ary sum can also be written as `sum(T1, T2, ..., Tn)`, but the binary
right-nested encoding is what the Yul code generator uses internally.

### Named Types

A named type is written as:

```
Name{T}
```

It attaches a human-readable label to an underlying structural type `T`. The
name is purely documentary: the Yul backend strips it and operates on the
underlying structure. Named types originate from user-defined `data`
declarations in Core Solidity; the specializer fills in the concrete type
arguments and wraps the result in a named type to preserve the original name in
diagnostic output.

For example, the Core Solidity declaration:

```
data Option(a) = None | Some(a);
```

when specialized at `a = word` produces the Hull named type:

```
Option{(unit + word)}
```

### Function Types

Function types are written as:

```
(T1, T2, ..., Tn -> R)
```

Function types appear only in function definitions; they are not first-class
values. Hull has no higher-order functions and no closures.

---

## Expressions

### Integer Literals

An integer literal is a non-negative decimal integer that denotes a `word`
value. The value must fit in 256 bits.

```
42
1000000
```

### Boolean Literals

`true` and `false` are the two values of type `bool`.

### Unit

The expression `()` is the single value of type `unit`.

### Variables

A variable refers to a locally declared name. Variable names follow the same
identifier rules as Yul: they may contain letters, digits, `_`, and `$`.

> **Note** The compiler conventionally uses names beginning with `$` for
> compiler-generated variables, such as `$alt` for the payload variable
> introduced by a `match` alternative. User-visible names do not begin with `$`.

### Pairs

A tuple expression constructs a product value:

```
(e1, e2)
```

The type of `(e1, e2)` is `(T1 * T2)` where `T1` and `T2` are the types of `e1`
and `e2`. Tuples with more than two elements are right-nested: `(e1, e2, e3)` is
equivalent to `(e1, (e2, e3))`.

### Projections

`fst(e)` evaluates to the first component of the pair `e`. `snd(e)` evaluates to
the second component.

```
fst(e)
snd(e)
```

Both `e` must have a product type.

### Sum Injections

A sum injection constructs a sum value. The following three forms are available:

| Form          | Meaning                                |
| ------------- | -------------------------------------- |
| `inl<T>(e)`   | Injects `e` as the left branch of `T`  |
| `inr<T>(e)`   | Injects `e` as the right branch of `T` |
| `in(k)<T>(e)` | k-th injection into the n-ary sum `T`  |

The type annotation `T` is the _target sum type_, not the type of `e`. This
annotation is mandatory: the Yul code generator needs the full sum type to
compute the correct memory representation of the injected value without
re-running type inference.

For example, `inr<Option{(unit + word)}>(42)` injects the word `42` into the
right branch of `Option{(unit + word)}`, representing `Some(42)`.
`inl<Option{(unit + word)}>(())` injects `()` into the left branch, representing
`None`.

### Function Calls

A function call applies a named function to a list of arguments:

```
f(e1, e2, ..., en)
```

All call targets in Hull are statically known names produced by the specializer.
There is no dynamic dispatch or virtual call mechanism.

### Conditional Expressions

A conditional expression selects between two alternatives based on a boolean
value:

```
if<T> cond then e1 else e2
```

`cond` must have type `bool`. Both `e1` and `e2` must have type `T`. The type
annotation `T` is mandatory. Conditional expressions are generated by the
if-desugaring pass from Core Solidity `if`/`else` expressions.

---

## Statements

Statements are executed sequentially inside function bodies and blocks. There
are no statement separators; statements are delimited by whitespace and the
structure of the enclosing block.

### Variable Declaration

```
let x : T
```

Declares a mutable local variable `x` of type `T`. The declaration does not
initialize the variable; it must be assigned before it is read. This corresponds
directly to Yul's uninitialized `let` declaration.

> **Warning** Reading a variable before it has been assigned is undefined
> behaviour at the Yul level. The compiler always assigns variables before
> reading them, but hand-written Hull must respect this invariant.

### Assignment

```
lhs := rhs
```

Assigns the value of `rhs` to `lhs`. The left-hand side is most commonly a
variable name, written as `x := e`.

### Expression Statement

A bare expression can be used as a statement:

```
e
```

The expression is evaluated for its side effects and the result is discarded.
This form is used for calls whose return type is `unit`.

### Return

```
return e
```

Returns the value of `e` from the enclosing function. Every execution path in a
Hull function must end with a `return` or `revert`.

### Block

```
{
  stmt1
  stmt2
  ...
}
```

A block groups a sequence of statements. Blocks are used as the bodies of
`match` alternatives and `function` definitions.

### Match

```
match<T> e with {
  inl $alt => { ... }
  inr $alt => { ... }
}
```

Deconstructs the sum-typed expression `e` of type `T`. Each alternative names
the payload of its branch and executes the corresponding block. The following
constructor patterns are available:

| Pattern | Matches                |
| ------- | ---------------------- |
| `inl`   | Left injection         |
| `inr`   | Right injection        |
| `in(k)` | k-th injection (n-ary) |

The type annotation `<T>` is mandatory. It is used by the Yul translator to
determine the memory layout of the sum type.

> **Note** Sum types with more than two constructors are represented as
> right-nested binary sums. Deconstructing them requires nested `match`
> expressions: the outer `match` separates the first constructor (left branch)
> from the remainder (right branch), and inner matches continue the
> decomposition.

### Function Definition

```
function f(x1 : T1, x2 : T2, ...) -> R {
  stmt1
  stmt2
  ...
}
```

Defines a named function with explicitly typed parameters and a single return
type. Hull functions are translated directly to Yul functions.

The following properties hold for all Hull functions:

- Parameters and the return type must be given explicit Hull types.
- Functions are first-order: no function-typed parameters or return values.
- Each function has exactly one return type.
- The specializer produces unique names for each monomorphic instantiation, for
  example `map$word` or `maybe$Word`.

### Assembly Block

```
assembly {
  <Yul statements>
}
```

An inline assembly block embeds raw Yul statements directly in the Hull output.
The block passes through unchanged into the generated Yul. Assembly blocks are
used to access EVM primitives such as `mload`, `mstore`, `add`, `iszero`,
`revert`, and similar opcodes.

See the [Yul documentation](https://docs.soliditylang.org/en/latest/yul.html)
for the complete overview of Yul syntax.

### Revert

```
revert "message"
```

Immediately aborts execution. The string literal is a diagnostic label. The
match compiler generates `revert` statements for branches that are unreachable
at the Core Solidity level but must be given a code path in the Hull output.

### Comment

```
/* text */
```

A block comment. Comments carry no semantics and are stripped during Yul
translation. The compiler inserts comments to annotate which source-level
constructor each `$alt` variable corresponds to, for example:

```
inl $alt => { /* None */
              return n
            }
```

---

## Alternatives

An alternative is a branch in a `match` statement:

```
Con $var => { body }
```

`Con` is one of `inl`, `inr`, or `in(k)`. `$var` is a fresh variable name that
is bound to the payload of the matched constructor. `body` is a block of
statements. The bound variable has the payload type of the matched branch: for
`inl` and `inr` on a sum type `(T1 + T2)`, the payload types are `T1` and `T2`
respectively.

---

## Contracts and Objects

At the top level, a Hull program consists of named _objects_ following the Yul
object model. An object has a name, a `code` block containing statements, and
zero or more inner objects:

```
object "ContractName" {
  code {
    ...deployment code...
  }
  object "ContractName_deployed" {
    code {
      ...runtime code...
    }
  }
}
```

The outer object's `code` block is the deployment (constructor) code. The inner
object's `code` block is the runtime code. This structure corresponds exactly to
the Yul object notation used by `solc`. During Hull emission, the Core Solidity
contract body is split into these two objects automatically.

---

## Concrete Syntax Reference

The grammar diagrams below use the railroad diagram convention: rounded boxes
denote terminals (keywords and punctuation literals) and rectangular boxes
denote non-terminals.

### Types

**PrimaryType** — the base forms that can appear in any type position:

![PrimaryType railroad diagram](diagrams/PrimaryType.svg)

**Type** — combines primary types with `*` (product) and `+` (sum), both
right-associative:

![Type railroad diagram](diagrams/Type.svg)

### Expressions

**PrimaryExpr** — literals, variables, tuples, and calls:

![PrimaryExpr railroad diagram](diagrams/PrimaryExpr.svg)

**Expr** — the complete expression grammar, including injections, projections,
conditionals, and primary expressions:

![Expr railroad diagram](diagrams/Expr.svg)

### Statements

**Stmt** — the complete statement grammar:

![Stmt railroad diagram](diagrams/Stmt.svg)

**Block** — a brace-enclosed sequence of statements:

![block railroad diagram](diagrams/Block.svg)

### Auxiliaries

**Arg** — a typed parameter in a function definition:

![Arg railroad diagram](diagrams/Arg.svg)

**Con** — a constructor tag in a match alternative:

![Con railroad diagram](diagrams/Con.svg)

**Alt** — a match alternative, binding the payload to a name:

![Alt railroad diagram](diagrams/Alt.svg)

### Objects

**Object** — a named code container, optionally containing inner objects:

![Object railroad diagram](diagrams/Object.svg)

---

## Examples

The following examples show Core Solidity source programs alongside the Hull
they produce. All examples are accepted by the Core Solidity prototype.

### Identity Function

The simplest possible Hull function passes its argument through unchanged:

```
function id(x : word) -> word {
    return x;
}
```

Hull output:

```
function id (x : word) -> word {
  return x
}
```

### Optional Value

The following Core Solidity program defines an `Option` type and a `maybe`
function that extracts the contained value or returns a default:

```
data Option(a) = None | Some(a);

function maybe(n : word, o : Option(word)) -> word {
    match o {
        | None    => return n;
        | Some(v) => return v;
    }
}
```

After specialization at `a = word`, the Hull output is:

```
function maybe$Word (n : word, o : Option{(unit + word)}) -> word {
  match<Option{(unit + word)}> o with {
    inl $alt => { /* None */
                  return n
                }
    inr $alt => { /* Some */
                  let var_1 : word
                  var_1 := $alt
                  return var_1
                }
  }
}
```

Note that the function is renamed from `maybe` to `maybe$Word` by the
specializer to reflect the type instantiation. The `Option{(unit + word)}` named
type preserves the original constructor name in the type annotation.

### Enumeration Type

The following program defines a three-constructor enumeration and converts it to
an integer:

```
data Color = Red | Green | Blue;

function fromEnum(c : Color) -> word {
    match c {
        | Red   => return 0;
        | Green => return 1;
        | Blue  => return 2;
    }
}
```

Hull output:

```
function fromEnum (c : Color{(unit + (unit + unit))}) -> word {
  match<Color{(unit + (unit + unit))}> c with {
    inl $alt => { /* Red */
                  return 0
                }
    inr $alt => match<(unit + unit)> $alt with {
                  inl $alt => { /* Green */
                                return 1
                              }
                  inr $alt => { /* Blue */
                                return 2
                              }
                }
  }
}
```

The three-constructor type `Color` is encoded as the right-nested binary sum
`(unit + (unit + unit))`. The outer `match` separates `Red` (left) from the
remaining constructors (right). An inner `match` then separates `Green` (left)
from `Blue` (right).

### EVM Arithmetic via Assembly

The following contract uses an inline assembly block to invoke the `add` EVM
opcode:

```
contract Add1 {
    function main() -> word {
        let res : word;
        assembly { res := add(40, 2) }
        return res;
    }
}
```

Hull output:

```
object "Add1" {
  code {
    ...deployment code...
  }
  object "Add1_deployed" {
    code {
      function main () -> word {
        let res : word
        assembly {
          res := add(40, 2)
        }
        return res
      }
    }
  }
}
```

The `assembly` block is reproduced verbatim in the Hull output and passes
through unchanged into the generated Yul.
