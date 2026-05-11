# Assembly Blocks

SAIL allows inline _assembly blocks_ that embed Yul statements directly in a
function body. Assembly blocks provide unrestricted access to EVM opcodes and
are the primary mechanism for operations that SAIL has no built-in syntax for,
such as memory reads and writes, cryptographic operations, and ABI encoding
helpers.

```solcore
function add(x : word, y : word) -> word {
    let res : word;
    assembly {
        res := add(x, y)
    }
    return res;
}
```

> **Warning** The type checker cannot verify the semantic correctness of Yul
> code. Incorrect assembly can produce contracts that silently compute wrong
> results or revert unexpectedly. Minimize the size of assembly blocks and
> document any non-obvious invariants.

---

## Yul Sublanguage

The contents of an `assembly { … }` block are written in
[Yul](https://docs.soliditylang.org/en/latest/yul.html), the low-level
intermediate language used by the Solidity compiler. Yul operates exclusively
on 256-bit machine words — the native value type of the EVM.

SAIL variables whose type is `word` are accessible by name inside the block.
Variables of other SAIL types cannot be referenced directly in Yul.

---

## Variable Declaration

Inside an assembly block, Yul variables are declared with `let` and assigned
with `:=`. Yul `let` is separate from SAIL `let`: Yul variables exist only
within the enclosing Yul block.

```solcore
assembly {
    let ptr := mload(0x40)   // Yul variable; exists only in this block
}
```

Multiple names may appear on the left-hand side of a single `let` to receive
the multiple return values of a built-in opcode:

```solcore
assembly {
    let success, returndata := call(gas(), target, value, argOffset, argSize, 0, 0)
}
```

---

## Assignment

An assignment in Yul uses `:=`. The left-hand side must be either a Yul
variable or a SAIL `word` variable in scope.

```solcore
function store(loc : word, val : word) -> () {
    assembly {
        mstore(loc, val)   // EVM opcode: write val to memory address loc
    }
}
```

Assigning to a SAIL variable communicates a result back to the SAIL scope:

```solcore
function get_free_ptr() -> word {
    let ptr : word;
    assembly {
        ptr := mload(0x40)
    }
    return ptr;
}
```

---

## Conditionals

Yul provides an `if` statement that executes a block when a condition is
non-zero. There is no `else` branch in Yul; use `switch` for multi-way
dispatch.

```solcore
assembly {
    if iszero(success) {
        revert(0, 0)
    }
}
```

---

## Switch

The Yul `switch` statement dispatches on a value. Each `case` arm matches a
literal. An optional `default` arm matches any value not handled by a `case`.

```solcore
assembly {
    switch selector
    case 0x70a08231 {
        // balanceOf selector
    }
    case 0xa9059cbb {
        // transfer selector
    }
    default {
        revert(0, 0)
    }
}
```

---

## For Loops

Yul's `for` statement provides a general loop with an initialisation block, a
condition expression, a post-iteration block, and a body block.

```solcore
contract YulFor {
    function main() -> word {
        let loopStart : word = 128;
        let loopEnd   : word = 256;
        let res : word;
        assembly {
            let i := loopStart
            for {} lt(i, loopEnd) { i := add(i, 32) }
            {
                mstore(i, 42)
            }
            res := mload(192)
        }
        return res;
    }
}
```

The initialisation block may be empty (`{}`). `break`, `continue`, and `leave`
control loop execution:

| Statement  | Effect                                      |
| ---------- | ------------------------------------------- |
| `break`    | Exit the innermost `for` loop immediately   |
| `continue` | Skip to the post-iteration block of the loop |
| `leave`    | Return from the enclosing Yul function      |

---

## Nested Blocks

An assembly block may contain nested Yul blocks `{ … }`. Variables declared
inside a nested block are not visible outside it.

```solcore
assembly {
    {
        let tmp := mload(0x00)   // tmp is scoped to this inner block
        mstore(0x20, tmp)
    }
    // tmp is not in scope here
}
```

---

## Accessing SAIL Variables

SAIL variables of type `word` that are in scope at the point of the `assembly`
block can be read and written directly by name inside Yul. Variables of any
other SAIL type are not directly accessible.

```solcore
function example(x : word) -> word {
    let result : word;
    assembly {
        result := add(x, 1)   // x is a SAIL parameter; result is a SAIL local
    }
    return result;
}
```

> **Note** Variables assigned inside an assembly block must have been declared
> with `let` in the enclosing SAIL scope before the block. Attempting to
> read a SAIL variable of a non-`word` type inside Yul is a type error.

---

## Common EVM Operations

The following table lists the EVM opcodes most frequently used in assembly
blocks. For the full list see the
[EVM opcode reference](https://www.evm.codes/).

| Opcode              | Description                                    |
| ------------------- | ---------------------------------------------- |
| `add(a, b)`         | 256-bit addition (wrapping)                    |
| `sub(a, b)`         | 256-bit subtraction (wrapping)                 |
| `mul(a, b)`         | 256-bit multiplication (wrapping)              |
| `div(a, b)`         | 256-bit unsigned integer division              |
| `mod(a, b)`         | 256-bit unsigned modulo                        |
| `mload(p)`          | Read 32 bytes from memory at offset `p`        |
| `mstore(p, v)`      | Write 32 bytes `v` to memory at offset `p`     |
| `sload(k)`          | Read storage slot `k`                          |
| `sstore(k, v)`      | Write value `v` to storage slot `k`            |
| `caller()`          | Address of the message sender                  |
| `callvalue()`       | Value (in wei) sent with the call              |
| `calldataload(p)`   | Read 32 bytes from calldata at offset `p`      |
| `iszero(x)`         | 1 if `x == 0`, else 0                          |
| `revert(p, s)`      | Abort execution; return `s` bytes from `p`     |
| `return(p, s)`      | Halt execution; return `s` bytes from `p`      |

---

## Assembly and the Hull IR

Assembly blocks pass through the compilation pipeline unchanged. The SAIL
compiler includes them verbatim in the Hull IR, and the Yul code generator
reproduces them without transformation. This means the programmer has full
control over the generated Yul but also bears full responsibility for its
correctness.
