# Built-ins

SAIL provides a small set of types, values, and operators that are available in
every source file without any import. These are wired into the compiler's
initial environment. In addition, every `assembly` block has access to the full
set of EVM opcodes through Yul primitives.

---

## Primitive Types

Five types are built into the language kernel.

| Type | Description |
| ---- | ----------- |
| `word` | 256-bit unsigned integer; the EVM's native machine word |
| `bool` | Boolean type with constructors `true` and `false` |
| `()` | Unit type; used as the return type of functions that produce no value |
| `pair a b` | Generic product type, also written `(a, b)` in tuple syntax |
| `sum a b` | Generic disjoint union with constructors `inl` and `inr` |

`pair` and `sum` are the internal representation of all user-defined algebraic
data types. A data type with multiple constructors is encoded as a nested `sum`,
and a constructor with multiple fields is encoded as a nested `pair`. User code
rarely names `pair` or `sum` directly; they appear implicitly through `data`
declarations and tuple syntax.

### `word`

`word` is the only numeric type at the kernel level. Every integer literal in
SAIL has type `word`. There is no numeric overloading: `42`, `0xff`, and
`0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff` are all
`word` values.

```solidity
function decimals() -> word {
    return 18;
}
```

### `bool`

`bool` has exactly two constructors, `true` and `false`, both of which are
always in scope.

```solidity
function isActive(paused : bool) -> bool {
    match paused {
    | false => return true;
    | true  => return false;
    }
}
```

### `()` — Unit

The unit type `()` has a single value, also written `()`. Functions that
perform side effects and return nothing use `()` as their return type.

```solidity
function setOwner(newOwner : word) -> () {
    assembly { sstore(0, newOwner) }
}
```

### Tuple Syntax

The syntax `(a, b)` is shorthand for `pair a b`. Tuples with more than two
elements are right-nested pairs: `(a, b, c)` means `pair a (pair b c)`.

```solidity
data Transfer = Transfer(word, word, word);

function unpack(t : Transfer) -> (word, word, word) {
    match t {
    | Transfer(from, to, amount) => return (from, to, amount);
    }
}
```

---

## Infix Operators

SAIL provides eight infix operators as syntactic sugar over ordinary function
calls. The parser rewrites each operator to the corresponding function call
before name resolution; the functions themselves must be in scope at the point
of use.

| Operator | Equivalent call | Type |
| -------- | --------------- | ---- |
| `e1 < e2`  | `lt(e1, e2)`  | `bool -> bool -> bool` |
| `e1 > e2`  | `gt(e1, e2)`  | `bool -> bool -> bool` |
| `e1 <= e2` | `le(e1, e2)`  | `bool -> bool -> bool` |
| `e1 >= e2` | `ge(e1, e2)`  | `bool -> bool -> bool` |
| `e1 != e2` | `ne(e1, e2)`  | `bool -> bool -> bool` |
| `e1 && e2` | `and(e1, e2)` | `bool -> bool -> bool` |
| `e1 \|\| e2` | `or(e1, e2)`  | `bool -> bool -> bool` |
| `!e`       | `not(e)`      | `bool -> bool` |

Because the operators desugar to function calls, the compiler resolves them
through the normal type class and name resolution machinery. The functions
`lt`, `gt`, `le`, `ge`, `ne`, `and`, `or`, and `not` are not built into the
kernel; they must be brought into scope before use.

> **Note** The `&&` and `||` operators do not short-circuit in the current
> implementation. Both operands are always evaluated before the logical
> operation is performed.

```solidity
import std.{lt, ge, and};

function isValidAmount(amount : word, balance : word) -> bool {
    return amount > 0 && amount <= balance;
}
```

---

## The `invokable` Class

The kernel defines one built-in type class:

```solidity
forall self args ret . class self:invokable(args, ret) {
    function invoke(self : self, args : args) -> ret;
}
```

`invokable` is the compiler's mechanism for encoding higher-order functions.
When a function-typed value is passed as an argument or stored in a data
structure, the compiler generates an `invokable` instance that captures the
closure and implements `invoke`. User code rarely interacts with `invokable`
directly. A dedicated chapter covers higher-order functions and the
defunctionalization transformation in detail; see
[Lambda Functions](../core/lambda-functions.md).

---

## Assembly Primops

Inside an `assembly { }` block, all EVM opcodes are available as Yul
primitives. Each opcode is treated as a function that operates exclusively on
`word` values. Variables declared in the surrounding SAIL scope are accessible
by name inside the block.

The sections below list every available opcode grouped by category, along with
its Yul type signature.

### Arithmetic

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `add(x, y)` | `word -> word -> word` | Addition modulo 2^256 |
| `sub(x, y)` | `word -> word -> word` | Subtraction modulo 2^256 |
| `mul(x, y)` | `word -> word -> word` | Multiplication modulo 2^256 |
| `div(x, y)` | `word -> word -> word` | Integer division; 0 if y = 0 |
| `sdiv(x, y)` | `word -> word -> word` | Signed integer division |
| `mod(x, y)` | `word -> word -> word` | Modulo; 0 if y = 0 |
| `smod(x, y)` | `word -> word -> word` | Signed modulo |
| `exp(x, y)` | `word -> word -> word` | x raised to the power y |
| `addmod(x, y, m)` | `word -> word -> word -> word` | (x + y) mod m |
| `mulmod(x, y, m)` | `word -> word -> word -> word` | (x * y) mod m |
| `signextend(b, x)` | `word -> word -> word` | Sign-extend from bit b |

```solidity
function checkedAdd(x : word, y : word) -> word {
    let result : word;
    let overflow : word;
    assembly {
        result   := add(x, y)
        overflow := lt(result, x)
    }
    if (overflow != 0) {
        assembly { revert(0, 0) }
    }
    return result;
}
```

### Bitwise

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `and(x, y)` | `word -> word -> word` | Bitwise AND |
| `or(x, y)` | `word -> word -> word` | Bitwise OR |
| `xor(x, y)` | `word -> word -> word` | Bitwise XOR |
| `not(x)` | `word -> word` | Bitwise NOT |
| `byte(n, x)` | `word -> word -> word` | nth byte of x (0 = most significant) |
| `shl(shift, value)` | `word -> word -> word` | Left shift |
| `shr(shift, value)` | `word -> word -> word` | Logical right shift |
| `sar(shift, value)` | `word -> word -> word` | Arithmetic right shift |

### Comparison

Comparison opcodes return 1 if the condition holds and 0 otherwise. The result
type is `word`, not `bool`; use `tobool` or a match on the result to convert.

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `lt(x, y)` | `word -> word -> word` | 1 if x < y (unsigned) |
| `gt(x, y)` | `word -> word -> word` | 1 if x > y (unsigned) |
| `slt(x, y)` | `word -> word -> word` | 1 if x < y (signed) |
| `sgt(x, y)` | `word -> word -> word` | 1 if x > y (signed) |
| `eq(x, y)` | `word -> word -> word` | 1 if x = y |
| `iszero(x)` | `word -> word` | 1 if x = 0 |

```solidity
function isOwner(account : word) -> bool {
    let owner : word;
    let result : word;
    assembly {
        owner  := sload(0)
        result := eq(account, owner)
    }
    if (result) {
        return true;
    } else {
        return false;
    }
}
```

### Hashing

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `keccak256(offset, size)` | `word -> word -> word` | Keccak-256 hash of `size` bytes starting at memory `offset` |

```solidity
function storageSlot(account : word) -> word {
    let slot : word;
    assembly {
        mstore(0, account)
        slot := keccak256(0, 32)
    }
    return slot;
}
```

### Memory

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `mload(p)` | `word -> word` | Load word from memory at offset p |
| `mstore(p, v)` | `word -> word -> ()` | Store word v to memory at offset p |
| `mstore8(p, v)` | `word -> word -> ()` | Store the least significant byte of v at offset p |
| `msize()` | `word` | Size of active memory in bytes |
| `mcopy(dst, src, size)` | `word -> word -> word -> ()` | Copy size bytes from src to dst |
| `memoryguard(n)` | `word -> word` | Declare minimum memory usage to the optimizer |

### Storage

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `sload(slot)` | `word -> word` | Load value from storage slot |
| `sstore(slot, value)` | `word -> word -> ()` | Store value to storage slot |

```solidity
function transfer(to : word, amount : word) -> () {
    let callerSlot : word;
    let toSlot : word;
    let senderBal : word;
    let recipientBal : word;
    assembly {
        callerSlot   := caller()
        toSlot       := to
        senderBal    := sload(callerSlot)
        recipientBal := sload(toSlot)
        sstore(callerSlot, sub(senderBal, amount))
        sstore(toSlot,     add(recipientBal, amount))
    }
}
```

### Call Data

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `calldataload(p)` | `word -> word` | Read 32 bytes from calldata at offset p |
| `calldatasize()` | `word` | Total size of calldata in bytes |
| `calldatacopy(dst, src, size)` | `word -> word -> word -> ()` | Copy calldata into memory |

### Return Data

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `returndatasize()` | `word` | Size of the most recent return data |
| `returndatacopy(dst, src, size)` | `word -> word -> word -> ()` | Copy return data into memory |

### Code

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `codesize()` | `word` | Size of the current contract's bytecode |
| `codecopy(dst, src, size)` | `word -> word -> word -> ()` | Copy bytecode into memory |
| `extcodesize(addr)` | `word -> word` | Bytecode size of external contract at addr |
| `extcodecopy(addr, dst, src, size)` | `word -> word -> word -> word -> ()` | Copy external bytecode into memory |
| `extcodehash(addr)` | `word -> word` | Keccak-256 hash of external contract's bytecode |
| `datasize(name)` | `string -> word` | Size of a named Yul data object |
| `dataoffset(name)` | `string -> word` | Offset of a named Yul data object |

### Control Flow

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `stop()` | `()` | Halt execution successfully |
| `return(offset, size)` | `word -> word -> a` | Return `size` bytes from memory at `offset` and halt |
| `revert(offset, size)` | `word -> word -> a` | Revert with `size` bytes from memory at `offset` |
| `invalid()` | `()` | Trigger the invalid opcode; consumes all remaining gas |
| `selfdestruct(addr)` | `word -> ()` | Destroy the contract and send balance to addr |

`return` and `revert` have a polymorphic return type `a` because execution does
not resume after them; they can appear in any expression position regardless of
the expected type.

### Stack and Program Counter

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `pop(v)` | `word -> ()` | Discard a value |
| `pc()` | `word` | Current program counter value |

### Gas

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `gas()` | `word` | Remaining gas for the current execution |

### External Calls

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `call(gas, addr, value, inOffset, inSize, outOffset, outSize)` | `word^7 -> word` | Call external contract; returns 1 on success |
| `callcode(gas, addr, value, inOffset, inSize, outOffset, outSize)` | `word^7 -> word` | Like `call`, but runs in the caller's context |
| `delegatecall(gas, addr, inOffset, inSize, outOffset, outSize)` | `word^6 -> word` | Delegatecall; preserves caller and value |
| `staticcall(gas, addr, inOffset, inSize, outOffset, outSize)` | `word^6 -> word` | Read-only external call |

### Contract Creation

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `create(value, offset, size)` | `word -> word -> word -> word` | Deploy new contract; returns address |
| `create2(value, offset, size, salt)` | `word -> word -> word -> word -> word` | Deploy with deterministic address |

### Logging

| Opcode | Signature | Description |
| ------ | --------- | ----------- |
| `log0(offset, size)` | `word -> word -> ()` | Emit log with no topics |
| `log1(offset, size, topic1)` | `word -> word -> word -> ()` | Emit log with 1 topic |
| `log2(offset, size, topic1, topic2)` | `word^4 -> ()` | Emit log with 2 topics |
| `log3(offset, size, topic1, topic2, topic3)` | `word^5 -> ()` | Emit log with 3 topics |
| `log4(offset, size, topic1, topic2, topic3, topic4)` | `word^6 -> ()` | Emit log with 4 topics |

```solidity
function emitTransfer(from : word, to : word, amount : word) -> () {
    let transferTopic : word;
    assembly {
        transferTopic := 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
        mstore(0x00, amount)
        log3(0x00, 0x20, transferTopic, from, to)
    }
}
```

### Blockchain Context

These opcodes expose information about the current transaction and block. All
return `word`.

| Opcode | Description |
| ------ | ----------- |
| `address()` | Address of the executing contract |
| `balance(addr)` | Ether balance of addr in wei |
| `selfbalance()` | Ether balance of the executing contract |
| `caller()` | Address of the direct caller (`msg.sender`) |
| `callvalue()` | Ether sent with the call in wei (`msg.value`) |
| `origin()` | Address that originated the transaction (`tx.origin`) |
| `gasprice()` | Gas price of the transaction |
| `chainid()` | Chain identifier |
| `basefee()` | Base fee of the current block |
| `blockhash(blockNumber)` | Hash of the given block (only last 256 blocks) |
| `coinbase()` | Beneficiary address of the current block |
| `timestamp()` | Unix timestamp of the current block |
| `number()` | Current block number |
| `difficulty()` | Difficulty of the current block |
| `prevrandao()` | Previous RANDAO value (post-Merge randomness source) |
| `gaslimit()` | Gas limit of the current block |

```solidity
function onlyOwner(ownerSlot : word) -> () {
    let owner : word;
    let msgSender : word;
    let isAuth : word;
    assembly {
        owner     := sload(ownerSlot)
        msgSender := caller()
        isAuth    := eq(msgSender, owner)
    }
    if (isAuth) {
        return ();
    } else {
        assembly { revert(0, 0) }
    }
}
```
