# Core Solidity

Core Solidity is the user-facing language. Everything a program can express at
this level is defined by _desugaring_ into [SAIL](../sail/README.md): the
surface constructs described in this section are syntactic conveniences and
library definitions layered on top of the functional core, not new primitives in
the compiler. A numeric type is an ordinary data type, an arithmetic operator is
a trait method, a data location is a type constructor, and a contract is a record
of storage fields plus a generated dispatcher. Understanding that correspondence
is the theme of this section.

Where the [SAIL](../sail/README.md) chapters describe the general mechanisms
(algebraic data types, generics, traits, modules, inference), the chapters here
describe how those mechanisms are used to recover the features a Solidity
programmer expects, and where Core Solidity deliberately diverges from Classic
Solidity.

The most visible divergence is that **Core Solidity has no inheritance**. Classic
Solidity uses `is`, `virtual`, `override`, and `super`, resolved by C3
linearization, both to share code and to lay out storage. Core Solidity replaces
that single mechanism with the orthogonal tools of the functional core: behavior
is shared through traits and generic functions, and state is composed from
modules and records rather than flattened from a base-contract hierarchy.

The chapters are:

- **Numeric Types** — the word-backed integer, address, boolean, and fixed-bytes
  types, and how integer literals acquire a type.
- **Arithmetic** — operators as trait methods, and the current (unchecked)
  semantics of word arithmetic.
- **Lambda Functions** — anonymous functions and closures, and how they are
  eliminated before type checking.
- **Data Locations** — `memory`, `storage`, and `calldata` as type constructors,
  and the collection types (`array`, `mapping`, dynamic arrays) that live in
  them.
- **Contracts** — storage fields, constructors, public methods, and the
  generated entry point.
- **ABI Encoding / Decoding** — the library-defined serialization that connects a
  contract to the outside world.
- **Cross-contract Calls** — a worked example of two contracts that call each
  other.

Every type and trait named in this section is provided by the standard library
and documented, with its full signature, in the
[Standard Library](../stdlib/README.md) reference.
