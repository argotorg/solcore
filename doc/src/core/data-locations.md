# Data Locations

The EVM keeps data in several distinct places — the stack, memory, storage, and
calldata — that are read and written with different opcodes and have different
lifetimes. In Classic Solidity the location of a value is a modifier attached to
a declaration (`memory`, `storage`, `calldata`). In Core Solidity a location is a
_type constructor_: `memory<t>`, `storage<t>`, and `calldata<t>` are ordinary
library types, each a word-sized handle tagged with the logical type `t` it
refers to.

```solidity
enum memory<t>   { memory(word) }    // a pointer into memory
enum storage<t>  { storage(word) }   // a storage slot
enum calldata<t> { calldata(word) }  // an offset into calldata
```

Each wraps a single `word` — a memory address, a slot number, or a calldata
offset — so a located value costs exactly one stack word, and the phantom
parameter `t` records what lives at the other end without any runtime footprint.
Making the location part of the type means the type checker, not a separate set
of rules, keeps a storage reference and a memory reference from being confused,
and lets each location provide its own read/write behavior through traits.

## Reading and writing storage

A storage reference is turned into a value, and a value written back, through a
small family of traits.

| Trait          | Role                                                         |
| -------------- | ------------------------------------------------------------ |
| `StorageType`  | `load`/`store` a value at a slot                             |
| `StorageSize`  | how many slots a type occupies                               |
| `CanStore`     | a located reference whose value can be loaded and stored     |
| `StorageCopy`  | copy a value from one storage reference to another           |

`CanStore` is the one that contract fields and mapping values are built on: an
instance `CanStore<storage<t>, t>` says that a `storage<t>` reference can be
`load`ed to a `t` and a `t` can be `store`d through it. Reading a field and
assigning to a field both desugar to `CanStore` method calls, so any type with a
`CanStore` instance — including user structs and enums whose fields are
themselves dynamic — can be a field or a mapping value, not only the fixed-slot
primitives.

## Collections

The collection types are also location-tagged handles.

```solidity
enum mapping<member, index> { mapping(word) }  // key-indexed storage
enum array<member>          { array(word) }    // a storage array
enum DynArray<t>            {}                  // a memory/calldata dynamic array
```

`DynArray<t>` is a phantom tag with no constructor of its own; it is always used
under a location, as `memory<DynArray<t>>` or `calldata<DynArray<t>>`. The
dynamic byte types `bytes` and `string` are phantom tags in the same way, used as
`memory<bytes>`, `memory<string>`, and their calldata forms.

### Surface syntax

The familiar Solidity spellings desugar to these constructors:

| Surface                       | Type                              |
| ----------------------------- | --------------------------------- |
| `uint256[]`                   | `memory<DynArray<uint256>>`       |
| `mapping(address => uint256)` | `mapping<address, uint256>`       |
| `bytes`, `string`             | `memory<bytes>`, `memory<string>` |

### Indexing

Index access `c[i]` and index assignment `c[i] = v` desugar to method calls on
two complementary traits, so a read and a write resolve independently:

- `RValueIdxAccess` gives the _value_ at an index (a read),
- `LValueIdxAccess` gives a _cell reference_ to the element (the target of a
  write), which is then stored through `CanStore`.

The standard library provides instances for memory dynamic arrays, storage
arrays, and storage mappings. Every bounds check lives in one place — the
element-cell lookup — so reads and writes cannot disagree about what is in range.
A calldata array is read-only: it has an `RValueIdxAccess` instance but no
`LValueIdxAccess`, so `arr[i] = v` on calldata is rejected at compile time.
Calldata arrays are also decoded _lazily_, element by element, rather than copied
into memory up front.

### Length, push, and construction

Dynamic arrays carry their length in their first word. The `Length` trait reports
it (`.length()`), and `Array`/`ArrayPush` provide growth and removal (`.push(…)`,
`pop`). A memory array is constructed from an array literal, which the compiler
desugars into an allocation (`arrayLitNew`) followed by one write per element:

```solidity
function build() returns (uint256) {
    let xs: uint256[] = [10, 20, 30];
    xs[0] = 7;
    return xs.length();                // 3
}
```

The underlying allocator (`allocateDynamicArray`) reserves `n + 1` words — the
length prefix plus the elements — and records the length in the first word. These
helpers are documented in
[Memory, Storage, and Collections](../stdlib/memory-storage.md#allocation).

## Why locations are types, not modifiers

Treating a location as a type constructor is what lets the whole storage and ABI
layer live in the standard library rather than the compiler. `load`, `store`,
indexing, length, and ABI decoding are all trait methods selected by the handle's
type, so they extend to user-defined types through ordinary instance resolution,
and a value's location is checked by the same inference that checks everything
else. It is also why Core Solidity can compose a contract's state from
independent modules instead of a base-contract hierarchy: a reusable component
can own a `storage<t>` reference (for instance, a slot derived from an
[ERC-7201](https://eips.ethereum.org/EIPS/eip-7201) namespace) and export
operations over it, and a contract uses the component by calling those operations
rather than by inheriting its storage layout.
