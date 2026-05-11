# Variable Declaration and Assignment

SAIL distinguishes two kinds of mutable state: _local variables_ declared
inside function bodies and _field variables_ declared inside contract bodies.
Local variables exist only for the duration of a function call; field variables
persist in contract storage across transactions.

---

## Local Variable Declaration

A local variable is introduced with the `let` keyword inside a function body.
The type annotation and the initialiser are both optional.

### Declaration without annotation or initialiser

```solcore
let x;
```

The compiler assigns a fresh type variable to `x` and infers its type from
subsequent uses. The variable must be assigned before it is read; the compiler
does not insert a default value.

### Declaration with a type annotation

```solcore
let res : word;
```

The type is fixed to `word` at the point of declaration. The variable is still
uninitialized; it must be assigned before use.

```solcore
function add(x : word, y : word) -> word {
    let res : word;
    assembly { res := add(x, y) }
    return res;
}
```

### Declaration with an initialiser

An initialiser provides a value at declaration time. The type may still be
omitted and will be inferred from the initialiser expression.

```solcore
let x = 42;          // type inferred as word
let y : word = 42;   // type annotation and initialiser together
```

Initialised declarations are useful when the right-hand side is an expression
whose type would otherwise be ambiguous:

```solcore
data Option(a) = None | Some(a);

function join(mmx : Option(Option(word))) -> Option(word) {
    let result = Option.None;   // type inferred as Option(word) from later uses
    match mmx {
    | Option.Some(Option.Some(x)) => result = Option.Some(x);
    | _                           => result = Option.None;
    }
    return result;
}
```

---

## Assignment

### Simple assignment

An assignment statement writes a new value to an existing variable or to a
contract field. The left-hand side must be an _lvalue_ — a name or an indexed
expression.

```solcore
x = expr;
```

The type of `expr` must match the declared type of `x`.

```solcore
contract Counter {
    count : word;

    function increment() {
        let next : word;
        next = count;
        count = next;
    }
}
```

### Compound assignment

The `+=` and `-=` operators combine a read, an arithmetic operation, and a
write in a single statement.

```solcore
count += 1;   // equivalent to count = count + 1
count -= 1;   // equivalent to count = count - 1
```

Compound assignment is most commonly used with contract fields:

```solcore
contract Counter {
    counter1 : word;
    counter3 : word;

    function main() -> word {
        counter1 += 1;
        counter3 += 2;
        return counter1 + counter3;
    }
}
```

---

## Contract Field Variables

A field variable is declared inside a contract body with a mandatory type
annotation and an optional initialiser. Fields are stored in contract storage
and retain their values between calls.

```solcore
contract Token {
    owner   : address;
    supply  : word;
    paused  : bool;
}
```

Fields are accessed and assigned by name from any function inside the same
contract. A field cannot be accessed from a free function.

```solcore
contract Token {
    supply : word;

    function mint(amount : word) {
        supply += amount;
    }

    function totalSupply() -> word {
        return supply;
    }
}
```

### Field initialisers

An optional initialiser sets the field's value at deployment time. It is
evaluated once when the contract is deployed.

```solcore
contract Counter {
    count : word = 0;
}
```

---

## Contextual Assignment

The left-hand side of an assignment may be any expression that denotes an
_lvalue_. When the expected type of the right-hand side is determined by the
left-hand side, the contextual constructor shorthand `.Constructor` can be
used on the right-hand side.

```solcore
data Option(a) = None | Some(a);

function main() -> Option(word) {
    let x : Option(word);
    x = .None;         // equivalent to Option.None
    return x;
}
```

---

## Conditional Statement

The `if` statement executes a block conditionally on a boolean expression.
An optional `else` branch handles the false case.

```solcore
if (condition) {
    // executed when condition is true
}

if (condition) {
    // true branch
} else {
    // false branch
}
```

Both branches must produce the same type if the `if` statement appears in a
context where a value is expected. When used purely for side effects the
types need only be consistent:

```solcore
contract Counter {
    count : word;

    function increment(active : bool) {
        if (active) {
            count += 1;
        }
    }
}
```

> **Note** The condition must be of type `bool`. SAIL does not implicitly
> convert `word` to `bool`. Use an explicit comparison (`x != 0`) when the
> condition originates from a `word` value.

---

## Expression Statements

Any expression may appear as a statement. The expression is evaluated for
its side effects and the result is discarded. This is the standard way to
call a function whose return type is `()`.

```solcore
function log(msg : word) -> () {
    assembly { mstore(0, msg) }
}

function main() {
    log(42);    // expression statement: result () is discarded
}
```

---

## Scope and Shadowing

Local variables are in scope from their declaration to the end of the
enclosing block. A variable declared in an inner block shadows an outer
declaration of the same name for the duration of that block.

```solcore
function example() -> word {
    let x : word = 1;
    {
        let x : word = 2;   // shadows outer x inside this block
    }
    return x;               // refers to the outer x; returns 1
}
```

> **Note** The compiler uses unique identifiers internally, so shadowing is
> safe and does not cause name collisions in the generated code.
