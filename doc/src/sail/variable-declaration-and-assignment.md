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
let bal: word;
```

The type is fixed to `word` at the point of declaration. The variable is still
uninitialized; it must be assigned before use.

```solcore
function loadBalance(account: word) returns (word) {
    let bal: word;
    assembly { bal := sload(account) }
    return bal;
}
```

### Declaration with an initialiser

An initialiser provides a value at declaration time. The type may still be
omitted and will be inferred from the initialiser expression.

```solcore
let amount = 100;        // type inferred as word
let fee: word = 3;       // type annotation and initialiser together
```

Initialised declarations are useful when the right-hand side is an expression
whose type would otherwise be ambiguous:

```solcore
enum Result {
    Ok(word),
    Err(word)
}

function safeTransfer(from: word, to: word, amount: word) returns (Result) {
    let result = Result.Err(0);   // type inferred as Result from constructor
    let bal: word;
    assembly { bal := sload(from) }
    if (gte(bal, amount)) {
        result = Result.Ok(amount);
    }
    return result;
}
```

---

## Assignment

### Simple assignment

An assignment statement writes a new value to an existing variable or to a
contract field. The left-hand side must be an _lvalue_: a name or an indexed
expression.

```solcore
x = expr;
```

The type of `expr` must match the declared type of `x`.

```solcore
contract Vault {
    balance: word;

    function deposit(amount: word) {
        let next: word;
        next = balance;
        balance = add(next, amount);
    }
}
```

### Compound assignment

The `+=` and `-=` operators combine a read, an arithmetic operation, and a
write in a single statement.

```solcore
balance += amount;   // equivalent to balance = balance + amount
balance -= amount;   // equivalent to balance = balance - amount
```

Compound assignment is most commonly used with contract fields:

```solcore
contract ERC20 {
    totalSupply: word;
    feePool: word;

    function mint(amount: word) {
        totalSupply += amount;
        feePool     += div(amount, 100);
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
    owner: word;
    supply: word;
    paused: bool;
}
```

Fields are accessed and assigned by name from any function inside the same
contract. A field cannot be accessed from a free function.

```solcore
contract Token {
    supply: word;

    function mint(amount: word) {
        supply += amount;
    }

    function totalSupply() returns (word) {
        return supply;
    }
}
```

### Field initialisers

An optional initialiser sets the field's value at deployment time. It is
evaluated once when the contract is deployed.

```solcore
contract Token {
    supply: word = 0;
}
```

---

## Contextual Assignment

The left-hand side of an assignment may be any expression that denotes an
_lvalue_. When the expected type of the right-hand side is determined by the
left-hand side, the contextual constructor shorthand `.Constructor` can be
used on the right-hand side.

```solcore
enum Result {
    Ok(word),
    Err(word)
}

function main() returns (Result) {
    let r: Result;
    r = .Ok(0);         // equivalent to Result.Ok(0)
    return r;
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

`if` is a statement, so use the conditional expression `condition ? yes : no`
when a value is required:

```solcore
contract Token {
    paused: bool;

    function transfer(to: word, amount: word) {
        if (paused) {
            assembly { revert(0, 0) }
        }
    }
}
```

> **Note** The condition must be of type `bool`. SAIL does not implicitly
> convert `word` to `bool`. Use an explicit comparison (`x != 0`) when the
> condition originates from a `word` value.

---

## For Loop

The `for` statement provides a C-style counted loop. Its header has three
clauses separated by `;`:

```
for ( ForInitStmt ; Condition ; ForPostStmt ) Body
```

The **condition** is any expression of type `bool`; the loop runs while it is
`true`.

### Initialisation clause

The init clause runs once before the first iteration. It may:

* Declare a new local variable (typed or untyped, with or without an initialiser):

  ```solcore
  for (let i = 0; i < 10; i = i + 1) { ... }
  for (let i: word; i < 10; i = i + 1) { ... }
  ```

* Assign to an already-declared variable:

  ```solcore
  let i: word;
  for (i = 0; i < 10; i = i + 1) { ... }
  ```

* Use a compound assignment or a plain expression.

### Post-iteration clause

The post clause runs after each iteration, before the condition is re-tested.
It accepts assignments, compound assignments, and expression statements, but
does not introduce new bindings:

```solcore
let i: word;
let s = 0;
let j = 1;
for (i = 0; i <= 0; i = i + 1) {
    s = j;
}
```

### Complete examples

**Accumulate a sum from 1 to 10:**

```solcore
import {Num, Add, Sub, Eq, Ord, Bounded, Typedef, le} from std;

contract Sum {
    function main() returns (word) {
        let s = 0;
        for (let i = 1; i <= 10; i = i + 1) { s = s + i; }
        return s;    // 55
    }
}
```

**Loop variable declared before the `for`:**

```solcore
contract Sum {
    function main() returns (word) {
        let i: word;
        let s = 0;
        for (i = 1; i <= 10; i = i + 1) { s = s + i; }
        return s;
    }
}
```

**Loop variable shadows an outer declaration:**

```solcore
contract Shadow {
    function main() returns (word) {
        let i = 100;
        let s = 0;
        for (let i = 1; i <= 10; i = i + 1) { s = s + i; }
        // i is 100 here again
        return s;
    }
}
```

**Nested `if` inside a `for` body:**

```solcore
contract ForInner {
    function main() returns (word) {
        let result = 0;
        for (let height = 0; height < 7; height = height + 1) {
            if (true) { result = height; } else {}
        }
        return result;
    }
}
```

### Scope rules

A variable declared in the **init clause** is in scope for the condition
expression, the post-iteration clause, and the entire body.

The loop body is its own block; declarations inside it do not escape to the
enclosing function.

> **Note** The condition must be of type `bool`. SAIL does not implicitly
> convert `word` to `bool`. Use an explicit comparison (`le(i, 10)` or
> `i <= 10`) when comparing integer counters.

---

## Expression Statements

Any expression may appear as a statement. The expression is evaluated for
its side effects and the result is discarded. This is the standard way to
call a function whose return type is `()`.

```solcore
function emitTransfer(from: word, to: word, amount: word) {
    assembly {
        mstore(0x00, amount)
        log3(0x00, 0x20, 0xddf252ad, from, to)
    }
}

function main(to: word, amount: word) {
    emitTransfer(caller(), to, amount);    // expression statement: result () is discarded
}
```

---

## Scope and Shadowing

Local variables are in scope from their declaration to the end of the
enclosing block. A variable declared in an inner block shadows an outer
declaration of the same name for the duration of that block.

```solcore
function computeFee(amount: word) returns (word) {
    let fee: word = 1;
    {
        let fee: word = div(amount, 100);    // shadows outer fee inside this block
    }
    return fee;                              // refers to the outer fee; returns 1
}
```

> **Note** The compiler uses unique identifiers internally, so shadowing is
> safe and does not cause name collisions in the generated code.
