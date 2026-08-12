# ABI Encoding / Decoding

Contracts talk to each other and to the outside world through the _contract
application binary interface_ (ABI). Function arguments arrive as ABI-encoded
calldata, return values leave as ABI-encoded memory, and both sides must agree
on the same byte layout. Core Solidity implements this layout the same way
Solidity does, following a "head and tail" scheme.

Unlike most languages, Core Solidity does not bake ABI encoding into the
compiler. The whole mechanism lives in the standard library as a small family of
_type classes_, so encoding follows the type of a value and extends to
user-defined types through the same instance-resolution machinery described in
[Type Classes](../sail/typeclasses.md). The compiler contributes only the
lower-level pieces that cannot be expressed in the language itself, namely
compile-time `keccak256` for selectors and the uniform runtime layout of sums
and products (see [Datatypes](../sail/datatypes.md#runtime-encoding)).

## Overview

Three cooperating classes describe how a type participates in the ABI:

| Class        | Responsibility                                           |
| ------------ | -------------------------------------------------------- |
| `ABIAttribs` | static or dynamic classification and head size of a type |
| `ABIEncode`  | writing a value into an ABI-encoded memory region        |
| `ABIDecode`  | reading a value back from memory or calldata             |

All three are exported from `std`, together with the two user-facing entry
points `abi_encode` and `abi_decode`. A type is ABI-encodable exactly when it is
an instance of these classes, and every primitive type shipped by the standard
library already is.

## Entry points

The function `abi_encode` serializes a value and returns it as an ordinary
`memory(bytes)`, that is, a length-prefixed `[length | data]` block like any
other bytes value. It reserves one word for the length, writes the encoded
region right after it, and stores the region's byte length in that leading word.

```solcore
forall ty . ty:ABIAttribs, ty:ABIEncode => function abi_encode(val : ty) -> memory(bytes);
```

Because the result carries its own length, it composes with the rest of the
bytes API: the caller can query it with `MemorySize.len` and
`MemoryPointer.ptr`, concatenate it, hash it, or return it, without tracking the
encoded size separately.

```solcore
function encodeAmount(amount : uint256) -> memory(bytes) {
    return abi_encode(amount);
}
```

The dual function `abi_decode` reads a value back. Because the result type
cannot be inferred from the input alone, the target type and the source reader
are passed as `Proxy` values (see
[Phantom Type Parameters](../sail/datatypes.md#phantom-type-parameters)).

```solcore
forall decodable reader ty decoded
    . decodable:HasWordReader(reader), ABIDecoder(ty, reader):ABIDecode(decoded)
=> function abi_decode(decodable : decodable, pty : Proxy(ty), prdr : Proxy(reader)) -> decoded;
```

Most programs never call `abi_decode` directly. The contract dispatcher decodes
incoming calldata automatically (see [Contract Dispatch](#contract-dispatch)
below), so hand-written decoding is only needed for lower-level work.

## Layout metadata: `ABIAttribs`

Before a value can be laid out, the encoder needs to know two things about its
type: whether the type is _static_ (its size is fixed and known in advance) or
_dynamic_ (its size depends on the value), and how many bytes it occupies in the
_head_ of the encoding. The class `ABIAttribs` records both.

```solcore
forall self . class self:ABIAttribs {
    function headSize(ty : Proxy(self)) -> word;
    function isStatic(ty : Proxy(self)) -> bool;
}
```

Both methods take a `Proxy(self)` rather than a value, because the answer
depends only on the type. A default instance classifies every type as static
with a 32-byte head, and concrete types override it as needed:

```solcore
instance uint256:ABIAttribs {
    function headSize(ty : Proxy(uint256)) -> word { return 32; }
    function isStatic(ty : Proxy(uint256)) -> bool { return true; }
}
```

Dynamic types such as `bytes`, `string`, and dynamic arrays report a 32-byte
head (they occupy a single offset word there) but are not static. Aggregates
combine their components: a pair adds the head sizes of its parts and is static
only when both parts are static.

## Head and tail layout

The standard ABI splits an encoded region into a _head_ and a _tail_. Static
values are written inline in the head. A dynamic value writes a 32-byte _offset_
into the head, pointing at the position in the tail where its actual bytes live.
Decoders follow that offset to recover the value. This indirection is what lets
a fixed-size head describe variable-size data.

```
head                                   tail
┌───────────┬───────────┬───────────┐  ┌──────────────────────────┐
│ uint256   │ offset  ──┼──────────────►│ length │ data ...        │
│ (inline)  │ (dynamic) │   ...     │  └──────────────────────────┘
└───────────┴───────────┴───────────┘
```

The encoder threads two cursors through the whole process, one advancing over
the head and one over the tail, so an arbitrarily nested value is written in a
single pass. This is the standard encoding, not the packed one: every static
slot is padded to a full 32-byte word. For the tight, unpadded layout used when
building hash preimages, see [Packed Encoding](#packed-encoding).

## Encoding: `ABIEncode`

A type is encodable when it implements `encodeInto`, which writes a value into a
memory region and returns the updated tail cursor.

```solcore
forall self . class self:ABIEncode {
    // basePtr : start of the encoded region
    // offset  : bytes from basePtr to the first free head slot
    // tail    : index of the first free tail byte
    function encodeInto(x : self, basePtr : word, offset : word, tail : word) -> word /* newTail */;
}
```

Static primitives write their word straight into the head slot and leave the
tail untouched:

```solcore
instance uint256:ABIEncode {
    function encodeInto(x : uint256, basePtr : word, offset : word, tail : word) -> word {
        let repx : word = Typedef.rep(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}
```

Dynamic instances such as `memory(bytes)` and `memory(string)` do more: they
store the relative offset into the head, copy the `[length | data]` block into
the tail, pad it up to a multiple of 32, and return the advanced tail cursor.
The standard library provides instances for `uint256`, `address`, `bool`,
`bytes4` (right-aligned), `bytes32`, `bytes`, `string`, dynamic arrays, tuples,
the unit type, pairs, and sums.

## Decoding: `ABIDecode` and word readers

Decoding reverses the process. It must work against two different byte sources,
memory and calldata, which are read with different opcodes. Core Solidity
abstracts over the two with the `WordReader` class.

```solcore
forall ty . class ty:WordReader {
    function read(reader : ty) -> word;
    function advance(reader : ty, offset : word) -> ty;
    function copyToMem(reader : ty, dst : word, cnt : word) -> ();
}
```

`MemoryWordReader` implements it with `mload` and `mcopy`, `CalldataWordReader`
with `calldataload` and `calldatacopy`. Every decoder is written once against
`WordReader` and therefore works uniformly on both sources.

The decoding class itself carries the decoded type in its result position:

```solcore
forall decoder decoded . class decoder:ABIDecode(decoded) {
    function decode(ptr : decoder, currentHeadOffset : word) -> decoded;
}
```

Decoders for value types validate the incoming word before wrapping it. Reading
a `bool` requires the raw word to be `0` or `1`, and reading an `address`
requires the upper 96 bits to be clear. A violation reverts with a dedicated
error selector, so malformed calldata is rejected rather than silently accepted.
Dynamic arrays over calldata are decoded _lazily_: the length and each element
are read on demand, which avoids copying an entire array into memory when only a
few elements are used.

## Aggregates: tuples, products, and sums

Tuples and constructor fields are products, and Core Solidity represents them as
right-nested pairs (see [Tuples](../sail/datatypes.md#tuples)). The pair
instances encode the left component at the current offset and the right
component at the offset advanced by the head size of the left, which reproduces
the ordinary struct-of-fields ABI layout. A separate `ABITuple` wrapper recovers
the flat-tuple grouping that pairing would otherwise hide, so a multi-argument
tuple is laid out as one ABI tuple rather than a chain of nested pairs.

Sum types (any data type with more than one constructor) are encoded with a
leading _tag_ word, `0` for the left injection and `1` for the right, followed
by the selected branch. A static sum places the tag and branch inline in the
head; a dynamic sum places a single offset word in the head and the
`[tag | branch]` block in the tail. This mirrors the uniform `inl` / `inr`
encoding that all algebraic data types share.

## User-defined types

Structs and enums encode and decode without any hand-written instances. The
mechanism reuses the generic-programming bridge:

1. The `Generic(rep)` class establishes an isomorphism between a user type and
   its _sums of products_ representation built from the primitives `sum`, pair,
   and unit.
2. Importing `std.ABIGeneric` brings the marker class `ABIDeriving` into scope,
   which signals the compiler to derive the instances.

```solcore
import std.{*};
import std.ABIGeneric.{*};

data Person = Person(address wallet, uint256 balance);

function encodePerson(p : Person) -> memory(bytes) {
    return abi_encode(p);
}
```

Given the import, the compiler auto-derives a `Generic` instance, a concrete
`ABIAttribs` instance, and a concrete `ABIDecode` instance for each local data
type. `ABIEncode` is obtained for free through the generic bridge. The user type
is then encoded exactly as its structural representation: constructors become
tagged sums and fields become products, so a struct lays out as its fields in
order and an enum lays out as a tagged union.

> **Note** `ABIEncode` and `ABIAttribs` are supplied generically, but
> `ABIDecode` cannot be a default instance: its `decode` returns a
> result-position type variable that the specializer cannot monomorphize on its
> own. This is why a concrete `ABIDecode` instance is emitted per type instead
> of a single generic one.

## Function selectors

A call is routed by its four-byte _selector_, the first four bytes of the
`keccak256` hash of the canonical function signature. The signature string is
built by the `SigString` class, which maps each type to its ABI name (`uint256`,
`address`, `bool`, `bytes`, `string`, `T[]`, and comma-joined products), and the
`Selector` class turns it into the four-byte prefix.

```solcore
instance Method(name, payability, args, rets, fn):Selector {
    function compute(prx : Proxy(Method(name, payability, args, rets, fn))) -> bytes4 {
        let hash = keccakLit(sigStr(Proxy : Proxy(name)) + "(" + sigStr(Proxy : Proxy(args)) + ")");
        return bytes4(shr(224, hash));
    }
}
```

The hash is computed with `keccakLit`, which the compiler folds during comptime
evaluation. A method's selector is therefore a constant baked into the contract,
with no runtime hashing cost.

## Contract dispatch

The three classes above come together in the contract entry point, which the
compiler injects automatically (this pass can be turned off with
`--no-gen-dispatch`). On each call the generated dispatcher:

1. reads the incoming selector from the first four bytes of calldata,
2. compares it against each method's compile-time selector in turn,
3. decodes the remaining calldata into the method's argument tuple with
   `abi_decode`,
4. calls the method,
5. encodes the result with `abi_encode` and returns it.

Because `abi_encode` yields a well-formed `memory(bytes)`, the last step is
direct: the dispatcher returns the encoded region using its length prefix
(`MemorySize.len`) and data pointer (`MemoryPointer.ptr`), with no need to
measure the allocated memory to recover the size.

Argument decoding uses a `CalldataWordReader`, so calldata is read in place
without first copying it to memory. Before a non-payable method runs, the
dispatcher checks that the call carries no value and reverts otherwise. If the
calldata is shorter than the selector plus the method's head, or if no selector
matches, control falls through to the contract's fallback.

The same information also drives the Solidity-style ABI JSON that the compiler
emits alongside the bytecode, so external tools see the familiar interface
description.

## Packed encoding

The standard head and tail encoding is what contracts use for calldata and
return data. A second, tighter layout is available for building hash preimages,
where every byte matters and offsets would be noise. The `concat` function
concatenates values with no padding, and `keccak256_` hashes the result:

```solcore
function commitment(a : bytes32, b : address) -> bytes32 {
    return keccak256_(concat(a, bytes32(Typedef.rep(b))));
}
```

This packed path is separate from `ABIEncode`. It is the building block used,
for example, to construct the EIP-712 typed-data digests in `std.eip712`, where
a struct hash is the `keccak256` of its type hash concatenated with its encoded
members.
