# Introducing Core Solidity

Solidity is the most widely used smart contract language. It is robust, trustworthy, and today
secures hundreds of billions of dollars of value. We are proud of this success, and its track
record of secure code generation. Users of Solidity will however be keenly aware of some of its
limitations. The type system often lacks the expressiveness to produce reusable library code or
enforce core safety properties. The language has very limited support for compile time evaluation.
Many features are implemented in an inconsistent manner, or do not always work as expected.

Fixing these limitations within the current implementation has proven difficult. Upgrades must be
made in a somewhat ad-hoc manner, and each new addition makes reasoning about the correctness of
subsequent changes more difficult. We did not feel confident that we would be able to safely extend
the language in this way to add the kind of features that our users were asking for, and that we
feel are required to keep up with the ever increasing scale of systems being developed in Solidity.

Core Solidity is our solution. It is a rebuild of the Solidity type system and compiler front/middle end that will:

- introduce powerful new features,
- provide a strong foundation for compiler correctness as we continue to extend the language in the future,
- empower library authors and support a community-driven process for language evolution,
- expand the capabilities of verification and analysis tooling.

In addition to growing and expanding the language, we will also be removing or reworking some
existing features. We are already certain that we will be removing inheritance entirely. Additional
changes are less certain, but we are considering potentially replacing or reworking features
like try/catch, libraries, function pointers, type conversion, and data locations.

We currently have a working prototype for Core Solidity. Most of the examples in this post typecheck
and can produce executable code. Some examples make use of yet to be implemented syntax and will compile in the future. Much of the core type theory is stable, but we want to add at least compile
time evaluation and modules before we will consider the type system finalized. Extensive work
remains to build out the standard library and reach feature parity with Classic Solidity.

## A Note on Syntax

Most of the work to date has been focused on the design and implementation of the type system, and the
associated code generation pipeline down to Yul. In order to avoid getting bogged down in
bikeshedding around syntax, and with the desire to validate our core ideas with a working
implementation as soon as possible, we moved ahead with a provisional syntax. You can expect
extensive changes before release. Our current intention is to eventually match the syntax of Classic
Solidity as much as possible. For any new syntax, you should expect that the final version will feel
closer to languages like TypeScript or Rust than it does right now.

## New Language Features

Core Solidity takes ideas from pure functional programming languages (e.g. Haskell, Lean), as well
as modern systems languages (e.g. Rust, Zig). We are extending Solidity with the following new
features:

- Algebraic datatypes (a.k.a sum / product types) and pattern matching
- Generics / parametric polymorphism
- Traits / typeclasses
- Type inference
- Higher order and anonymous functions
- Compile time evaluation

We think that taken together these core primitives will enable developers to produce stronger
abstractions, write more modular and reusable code, and leverage the type system to enforce core
safety properties.

We will continue to support the kind of low level access to the EVM that is often required by
production implementations: assembly will remain a core primitive, and we will extend it with the
ability to directly call functions from the high level language in assembly blocks. Users will be
able to disable the languages built in abstractions (e.g. contract dispatch generation, ABI
decoding, default storage layout generation), following the "pay for what you use" philosophy of
languages like Rust and C++.

### Algebraic data types and pattern matching

Algebraic Data Types provide a principled foundation for data modeling through the composition of
sum and product types. Sum types represent exclusive alternatives: a value inhabits exactly one
variant. Product types combine multiple values into structured tuples. These two primitives can
be combined to define precise types that make invalid states completely
unrepresentable, allowing the type system to enforce core program invariants entirely at
compile time.

Lets start with a very simple type:

```solidity
data Bool = True | False
```

Types are sets of values. The left hand side of the above statement defines the name of a new type
(`Bool`), and the right hand side defines the set of allowed values of the `Bool` type (`True` or
`False`). The `|` operator lets us introduce new alternative value constructors.

Alternatives can also hold values. This lets us implement the same kind of patterns as [User Defined
Value Types](https://docs.soliditylang.org/en/latest/types.html#user-defined-value-types) would in
Classic Solidity. We can consider a simple 18 decimal fixed point (a `wad`):

```solidity
data wad = wad(uint256)
```

The `wad` type has a single value constructor: `wad` (type names and value constructors live in
separate namespaces and so can share names) that holds a `uint256` as it's underlying
representation. Simple wrapper types like this will be erased by the compiler during the translation
into Yul, meaning that `wad` has the exact same runtime representation as a `uint256`.

Now we can define a type safe fixed point multiplication routine. We will need to extract the
underlying `uint256`, manipulate it, and then wrap it up again in a new `wad` constructor. To unwrap
we will use pattern matching. Pattern matching is a control flow mechanism that lets us destructure
and inspect data by shape. Instead of nested if-else chains, we can write declarative
expressions that exhaustively consider all possible values of the matched type.

For our simple `wad` example, we won't have any branches, but we can still use pattern matching to
destructure our inputs and extract their underlying representation:

```solidity
let WAD = 10 ** 18;

function wmul(lhs : wad, rhs : wad) -> wad {
    match (lhs, rhs) {
    | (wad(l), wad(r)) => return wad((l * r) / WAD);
    }
}
```

Lets look at a more complete example. Consider the following type definition for an auction state:

```solidity
data AuctionState =
    NotStarted(uint256)
  | Active(uint256, address)
  | Ended(uint256, address)
  | Cancelled(uint256, address);
```

`AuctionState` has four alternative value constructors: `NotStarted` specifies that the auction has
not yet started and stores its reserve price, `Active` denotes that the auction has begun and it
stores the current highest bid and the address that made that bid, `Ended` represents an auction
that has finished successfully and holds the highest bid and the address of the winner, and
`Cancelled` represents a cancelled auction, holding the highest bid and winning address at the time
of cancellation.

Now we can define a `processAuction` function that transitions the state based on the current state
and `msg.value`. The `match` statement lets us perform an exhaustive case analysis over each
possible alternative state. The `_` case at the end of the match is a default that handles any
remaining states that have not yet been explicitly matched. Exhaustiveness can be enforced by the
compiler, ensuring that every possible state is handled exactly once.

```solidity
function processAuction(state: State) -> State {
    match state {
    | NotStarted(reserve) =>
        require(msg.value >= reserve);
        return Active(msg.value, msg.sender);
    | Active(currentBid, bidder) =>
        require(msg.value > currentBid);
        transferFunds(bidder, currentBid);
        return Active(msg.value, msg.sender);
    | _ => return state;
    }
}
```

### Generics and type classes

Core Solidity introduces two new mechanisms for code sharing and polymorphism: generics and
type classes.

Generics implement parametric polymorphism: they enable us to write functions and data structures
that behave in a uniform way for all types. As an example, we can define a polymorphic `identity`
function:

```solidity
forall T . function identity(x : T) -> T {
    return x;
}
```

We can also define generic types, like this this `Result` type that is parameterised by the type of
the payload in the error case:

```solidity
data Result(T) = Ok | Err(T)
```

Generics are powerful, but by themselves quite limited. Most interesting operations are not defined
for all types. Type classes (a.k.a Traits) are the solution: they let us define an overloaded type
specific implementation for the same function name, and when combined with class constraints let us
define generic functions that are polymorphic over a restricted subset of types.

A type class is simply an interface specification. Consider the following definition of a class of
types that can be multiplied:

```solidity
forall T . class T:Mul {
    function mul(lhs : T, rhs : T) -> T;
}
```

Instead of the concrete `wmul` function that we defined above for our `wad` fixed point type, it
would be more idiomatic to defined an instance (a.k.a `impl`) of the `Mul` type class for `wad`
(giving us a uniform syntax for multiplication across all types, and allowing us to use our `wad`
type in functions that are generic over any instance of the `wad` type class):

```solidity
instance wad:Mul {
    function mul(lhs : wad, rhs : wad) -> wad {
        return wmul(lhs, rhs);
    }
}
```

If we want to write a function that can accept any type that is an instance of `Mul` we need to add
a constraint to the signature:

```solidity
forall T . T:Mul => function square(val : T) -> T {
    return Mul.mul(val, val);
}
```

Simple wrapper types like `wad` are very common, and one type class that can be particularly helpful
when working with them is `Typedef`:

```solidity
forall T U . class T:Typedef(U) {
    function abs(x:T) -> U;
    function rep(x:U) -> T;
}
```

The `abs` (abstraction) and `rep` (representation) functions let us move between wrapper types and
their underlying types in a generic way and without the syntactic noise of having to introduce
pattern matching every time we want to unwrap a type. The instance for `wad` would look like this:

```solidity
instance wad:Typedef(uint256) {
    function abs(u: uint256) -> wad {
        return wad(u);
    }

    function rep(x: wad) -> uint256 {
        match x {
        | wad(u) => return u;
        }
    }
}
```

Note that `U` parameter in the above `Typedef` definition is "weak": its value is uniquely
determined by the value of the `T` parameter. If you are familiar with Haskell or Rust, this is
effectively an associated type (although for any type system nerds reading, we implement it using a
restricted form of functional dependencies). To put it more plainly, we can only implement a single
instance of `Typedef` for `wad`: the compiler would not allow us to implement both
`wad:Typedef(uint256)` and `wad:Typedef(uint128)`. This restriction makes type inference much more
predictable and reliable by sidestepping many of the potential ambiguities inherent to full
multi-parameter type classes.

For a real world example of how generics and type class constraints can be used to eliminate
boilerplate or repetitive code, compare the combinatorial explosion of overloads required for the
[`console.log` implementation](https://github.com/foundry-rs/forge-std/blob/master/src/console.sol)
in `forge-std` to the following generic Core Solidity function that covers the functionality of all
the single argument overloads from the original library. The `word` type used in this implementation
is a low level type that represents a Yul variable, and is the only type that can be passed into or
out of assembly blocks.

```solidity
forall T . T:ABIEncode => function log(val : T) {
    let CONSOLE_ADDRESS : word = 0x000000000000000000636F6e736F6c652e6c6f67;
    let payload = abi_encode(val);

    // extract the underlying word representation of the payload
    let ptr = Typedef.rep(payload);

    assembly {
        pop(
            staticcall(
                gas(),
                CONSOLE_ADDRESS,
                add(ptr, 32),
                mload(ptr),
                0,
                0
            )
        )
    }
}
```

Similarly to Rust and Lean, all invocations of type classes and generic functions are fully
monomorphized at compile time, meaning polymorphic functions do not incur a runtime overhead when
compared to fully concrete functions. While this does mean that the compiled EVM code will
potentially contain multiple specialized versions of the same generic function, this does not entail
a binary size overhead compared to Classic Solidity which would anyway require multiple function
definitions for equivalent functionality. We consider this to be the correct tradeoff for our
domain.

### Higher-order and anonymous functions

Functions possess first-class status within the type system, enabling their use
as parameters, return values, and assignable entities.

As an example, consider the following which implements a custom ABI decoding
of a triple of booleans from a single `word` value:

```solidity
forall T . function unpack_bools(bools : word, fn : (bool, bool, bool) -> T) -> T {
    let b0 : bool = toBool(and(bools, 0x1));
    let b1 : bool = toBool(and(shr(1, bools), 0x1));
    let b2 : bool = toBool(and(shr(2, bools), 0x1));
    return fn(b0, b1, b2);
}
```

Function `unpack_bools` extracts three individual boolean values from the lowest three bits
of a packed `word` (`bools`). It treats it as a bitmask where the least significant bit
(position 0) represents `b0`, the next bit (position 1) represents `b1`, and the third bit
(position 2) represents `b2`. After extracting these bits and converting them to booleans,
it passes them as arguments to a callback function `fn` and returns whatever result
that function produces.

We also support the definition of (non-recursive) anonymous functions using the `lam` keyword.
Functions defined in this way can capture values available in the defining scope. As an example
consider this testing utility that counts the number of times an arbitrary function is called:

```solidity
forall T U . function count_calls(fn : (T) -> U) -> (memory(word), (T) -> U) {
    let counter : memory(word) = allocate(32);
    return (counter, lam (a : T) -> {
        counter += 1;
        return fn(a);
    });
}
```

Our implementation here is similar to systems languages like Rust and C++: the compiler produces a
unique type for each anonymous function that contains the captured state, and these unique types are
made callable by making them instances of the built in `invokable` type class (similar to the `Fn`
trait in Rust). This approach avoids the kind of dynamic heap based environment management used in
higher level languages like Haskell or Python.

### Type inference

Core Solidity supports the inference of types in almost any position. Annotations are usually optional and
need be added only where considered desirable for readability or understanding. Inference is decidable,
and the situations in which ambiguities requiring annotation can occur are very limited. This lets
us solve a lot of the syntactic clutter required when writing Solidity today. As an example,
consider the following Classic Solidity definition:

```solidity
struct LendingPosition {
    uint256 collateralDeposited;
    uint256 debtIssued;
    uint256 lastUpdated;
}
```

Instantiating `LendingPosition` in Classic Solidity is syntactically clunky, requiring us
to both annotate the type and invoke the `LendingPosition` constructor:

```solidity
LendingPosition memory emptyPosition = LendingPosition(0, 0, block.timestamp);
```

Since we are using the `LendingPosition` constructor, the compiler can infer
the type of `emptyPosition`. Core Solidity allows us to omit the redundant annotation:

```solidity
let emptyPosition = LendingPosition(0, 0, block.timestamp);
```

Assigning an expression to a variable in Classic can often result in redundant annotation if those
types are already present in the expression being assigned:

```solidity
(bytes memory a, bytes memory b) = abi.decode(input, (bytes, bytes))
```

The same definition is much cleaner in Core:

```solidity
let (a, b) = abi.decode(input, (uint256, uint256))
```

Another common frustration with Classic Solidity is the syntactic noise required when defining array
literals. Consider the following snippet:

```Solidity
uint256[3] memory a = [1, 2, 3];
```

This declaration is rejected by the compiler with the following error message:

```
Error: Type uint8[3] memory is not implicitly convertible to expected type uint256[3] memory.
```

Classic Solidity implements a limited and special cased form of type inference for numeric array
literals: it selects the smallest possible element type that can represent every element of the
array (in this case `uint8`). The compiler then throws a type error when attempting to assign this
value to a variable with a different type.

In order for the previous definition be accepted, we can add a type coercion to the first array element:

```solidity
uint256[3] memory a = [uint256(1), 2, 3];
```

The constraint based inference algorithm in Core Solidity is a lot more general, and will allow us
to omit this coercion:

```solidity
uint256[3] memory a = [1, 2, 3];
```

### Compile Time Evaluation

We do not yet have a prototype implementation of compile time evaluation, so don't have concrete
examples to share here yet. We are however very convinced that this will be a particularly valuable
extension to the language and consider it a critical feature that needs to be in place before
release. A strong goal is to minimize the differences between the runtime and compile time variants
of the language, allowing for a familiar syntax and code sharing between the two contexts.

Non-trivial design and implementation work remains here. We are exploring the degree to which access
to memory at compile time is required, and if it is, what kind of analysis passes we would want to
implement to guard against accidental leakage of references to compile time memory. We are also
investigating what kind of compile time specific primitives we might want to add, and whether we
want to expand the languages capabilities around reflection.

We will publish more on our designs once they stabilise. We want to make sure the needs of the
community are met here: if you have concrete real world use cases in mind for this feature, we would
be very interested to hear them.

## SAIL, Desugaring and the Standard Library

In addition to expanding the surface language, the transition to Core Solidity will also introduce a
new user accessible mid level IR: SAIL (Solidity Algebraic Intermediate Language). This is the
"Core" in Core Solidity. It's the most minimal language we could conceive of that would let us
express the full range of high-level language constructs found in Classic Solidity. It can usefully
be thought of as Yul with generics and type classes. It consists of the following primitive constructs:

- Functions
- Contracts
- Assembly (Yul) blocks
- Simple variable introduction and assignment
- A short circuiting if-then-else expression
- Algebraic datatypes & pattern matching
- Typeclasses
- Generics

It has a single builtin type (`word`) that has the same range of values as a Classic Solidity
`bytes32` or `uint256`, and can semantically can be viewed as "an item on stack" or "a yul
variable". Contracts in SAIL are very low level (essentially just a runtime entrypoint and initcode
entrypoint).

Although our current implementation uses Yul as an assembly language, this choice is largely
arbitrary from a theoretical standpoint, and it could also be instantiated over e.g. a RISCV based
assembly language instead.

We believe that SAIL is expressive enough that we can implement all other high level language
features and type as a combination of standard library definitions and desugaring passes (compile
time syntactic transformations into SAIL primitives). Core Solidity is then simply SAIL extended
with additional syntax sugar and libraries. It is similar to Yul in its dual function as both a compiler
IR and a user facing low level language, and the full range of SAIL primitives will be directly
available when writing Core Solidity. This style of language construction is often used in other
high assurance domains (e.g. theorem provers), and we believe it has important benefits for both
users of the language and the safety and security of it's implementation.

SAIL is simple enough that we expect to be able to construct an executable formal semantics for it.
This will allow us to mathematically guarantee core properties of the Solidity type system, provide
a reference implementation for differential fuzzing of the production implementation, and formally
verify both the standard library and higher-level language constructs. We believe that this will be
an essential part of our correctness story as both the language and the scale of the systems it is
used to construct continue to grow.

Library authors will have (almost) the same expressive power as the language designers, and will
be able to create abstractions that feel built-in to the language itself (a "library-based
language"). It will be possible to define and use alternative standard library implementations, or
disable the standard library completely. With the standard library disabled, it will be possible to
write Core Solidity code with almost the same level of control as low level assembly languages like
Yul or Huff, but with a modern, expressive type system, based on a mathematically rigorous foundation.

We also expect that the introduction of SAIL will make it much easier for Solidity users to extend
and improve the language. In many cases it will be possible to make deep improvements via a pull
request to the standard library alone. When new syntax or desugaring passes are required, we expect
them to be much easier to prototype and specify in SAIL without requiring knowledge and
understanding of compiler internals. We hope that SAIL and Core Solidity will allow us transition to
a community driven RFC style process for changes to the high level language and standard library.

### A Userspace `abi.encode`

One instructive example is the Core Solidity implementation of `abi.encode`. This is a complicated
and highly generic function that is currently provided as a compiler builtin. A full in language
implementation would not be possible in Classic Solidity due to the recursive nature of the ABI
specification (and the resulting infinite number of expressible types). The implementation presented
here is relatively concise, but does make use of some more advanced patterns and features. We
want to emphasise that existing users of Solidity will be able to be productive and make use of
their existing knowledge without having to concern themselves with these kind of low level internal
details. We hope that expert level users and library authors will however be excited by the new
potentials these features enable.

#### `uint256`

To begin we will construct the type `uint256`. In Classic Solidity the definition of this type and
it's associated operations are all built in language constructs. In Core, it is defined entirely in
language as a simple wrapper around a `word`. We also define a `Typedef` instance for it:

```solidity
data uint256 = uint256(word);

instance uint256:Typedef(word) {
    function abs(w: word) -> uint256 {
        return uint256(w);
    }

    function rep(x: uint256) -> word {
        match x {
        | uint256(w) => return w;
        }
    }
}
```

#### `memory` and `bytes`

Similarly we can build types that represent pointers into the various EVM data regions by again wrapping a
`word`. Notice that in the following snippet the type parameter on the memory pointer is *phantom*
(i.e. it appears only in the type, but is not mentioned in any of the value constructors). This is a
common idiom in ML family languages like Haskell or Rust that lets us enforce compile-time
constraints without runtime overhead.

```solidity
data memory(T) = memory(word)
```

The `bytes` type in Classic Solidity represents a tightly packed byte array with a size only known
at runtime. It doesn't really make sense to have a `bytes` on stack (and Classic Solidity always
requires that a data location be specified for a value of type `bytes`), so in Core we define it as
an empty type with no value constructors. Since `bytes` has no value constructors, we cannot produce
a value of `bytes` at runtime, and must always work with references to `bytes` (e.g. `memory(bytes)`).
This results in the same behaviour as Classic (users must always specify a data location), using
only SAIL primitives.

```solidity
data bytes;
```

Notice that in this construction of pointers and data locations, the data location is attached to
the type (instead of the variable binding as it is in Classic), allowing for the definition of e.g.
memory structures containing references to storage.

#### The `Proxy` type

The last piece of machinery required for `abi.encode` is the `Proxy` type:

```solidity
data Proxy(T) = Proxy;
```

As with the `memory` definition, the type parameter here is phantom, but unlike memory `Proxy`
carries no additional information at runtime. It exists only as a marker type that lets us pass
information around at compile time. Types like this are completely zero cost (i.e. they are
completely erased at runtime and do appear in the final compiled program at all).

Although somewhat esoteric, `Proxy` is very useful and gives us a lot of control over type inference
and instance selection without needing to pass data at runtime where it is not needed. It is often
used in both Haskell (where it is also called `Proxy`) and Rust (`std::marker::PhantomData`).

#### `abi.encode`

Now we are ready to implement Classic Solidity's `abi.encode` in SAIL. We start by defining a
typeclass for ABI related metadata, note that since this class does not need to care about the
actual value of the type being passed to it, we use a `Proxy` to keep our implementation as lean as
possible.

```solidity
forall T . class T:ABIAttribs {
    // how many bytes should be used for the head portion of the abi encoding of `T`
    function headSize(ty : Proxy(T)) -> word;
    // whether or not `T` is a fully static type
    function isStatic(ty : Proxy(T)) -> bool;
}

instance uint256:ABIAttribs {
    function headSize(ty : Proxy(uint256)) -> word { return 32; }
    function isStatic(ty : Proxy(uint256)) -> bool { return true; }
}
```

Now we define another class that handles the low level encoding into memory. The class presented
here contains some extraneous details needed for encoding compound and dynamic types that are not be
necessary for the simple `uint256` encoding we are implementing now. We present the full complexity
to demonstrate that we have the machinery required for this harder cases.

```solidity
// types that can be abi encoded
forall T . T:ABIAttribs => class T:ABIEncode {
    // abi encodes an instance of T into a memory region starting at basePtr
    // offset gives the offset in memory from basePtr to the first empty byte of the head
    // tail gives the index in memory of the first empty byte of the tail
    function encodeInto(x:T, basePtr:word, offset:word, tail:word) -> word /* newTail */;
}

instance uint256:ABIEncode {
    // a unit256 is written directly into the head
    function encodeInto(x:uint256, basePtr:word, offset:word, tail:word) -> word {
        let repx : word = Typedef.rep(x);
        assembly { mstore(add(basePtr, offset), repx) }
        return tail;
    }
}
```

Finally we can define a top_level `abi_encode` function that handles the initial memory allocation
and free memory pointer updates (we have omitted the implementation of the low level
`get_free_memory` and `set_free_memory` helpers for the sake of brevity):

```solidity
// top level encoding function.
// abi encodes an instance of `T` and returns a pointer to the result
forall T . T:ABIEncode => function abi_encode(val : T) -> memory(bytes) {
    let free = get_free_memory();
    let headSize = ABIAttribs.headSize(Proxy : Proxy(T));
    let tail = ABIEncode.encodeInto(val, free, 0, Add.add(free, headSize));
    set_free_memory(tail);
    return memory(free);
}
```

## Compatibility and Interoperability

Introducing such a major revision to any programming language is challenging. While a certain degree
of breakage is inevitable (and even desired), we want to make the transition as smooth as possible
and avoid a split in the language.

As with previous breaking upgrades to Solidity, ABI compatibility will be maintained between
versions, allowing individual contracts written in incompatible versions to interoperate and live
side by side in the same project (this strategy is also used by Rust with their "Editions" feature).
We are also investigating the feasibility of deeper interoperability beyond just the contract ABI.
We expect that it will be possible to share at least free functions and interface definitions
between language versions.

While there will be breakage of both syntax and semantics, our intention is to minimize it to cases
where it is either strictly necessary or brings significant benefits that justify the transition
cost. We expect that simple code that does not use inheritance will look and feel very similar in
both language versions, with only minor syntactic differences (largely just the switch from prefix
to postfix types). We are also considering reworking or replacing some features that have proven to
be problematic or limiting in practice (e.g. try/catch, libraries, function pointers, data
locations). Users can expect that some moderate changes may be required to adapt their code that
makes use of these features. Code that makes heavy use of inheritance will of course require the
largest changes.

We will be investigating the potential for automated upgrades, and if reliable and robust
implementations are possible expect to ship such tools at release.

Avoiding a Python 2 -> Python 3 style split is top of mind, and we believe that upgrades should be
manageable and that it will be possible to carry them out in an incremental manner.

## The Road to Production

This section outlines our current thinking on achieving production readiness and our strategy for
making such deep changes to the language in a safe way. Please note that this is a tentative plan,
and may be subject to extensive change. We are not yet in a position where we feel confident about
committing to concrete timelines. We will provide more details as
we get closer to a production implementation.

We have a prototype implemented in a [separate repository](https://github.com/argotorg/solcore). We
can typecheck SAIL programs, and have a code generation pipeline down to Yul implemented. We still
want to implement at least compile time evaluation and a module system before we will consider the
type system to be finalized. We have a rudimentary standard library implemented, and enough
desugaring stages built out to implement the most fundamental features of Classic Solidity. We can
produce ABI compatible contracts, with dispatch, abi encoding / decoding, and storage access.

There is still significant work remaining at the prototype stage before we can begin to consider a
full production implementation. We want to finalize the type system, flesh out the standard library,
and write enough code to be confident that what we have is sufficient to support the full range of
features that we think are necessary. We need to thoroughly document the typesystem and compiler
internals. We also expect to spend time working with existing power users and library authors to
gather feedback and make any necessary changes.

Once we are confident that the prototype is stable, work will split into two parallel streams:

1. Production implementation: we will reimplement the typechecker, desugaring and yul generation
   passes in a systems language (e.g. Rust, C++, Zig), and integrate it into solc proper. This
   implementation will focus on correctness, performance, and providing the best possible
   diagnostics and error messages.
2. Executable Formal Semantics: we will work to mechanize our existing latex specification in a
   theorem proving environment (likely Lean). This will be used to build confidence in our
   production implementation, as well as the standard library and typesystem itself.

Once the production implementation is relatively stable. There will be a period of time in which
Core Solidity is available as an experimental feature, but not yet marked as production ready. We
will use this period to gain real world feedback from our users, continue fuzzing, and put the
standard library out for external review. When we are confident that the new pipeline is free of
major faults, we will release a breaking version of solc with Core as the default language version.

## Beyond 1.0

Our focus right now is to deliver the language as described in this post. This is a significant
undertaking, and not one that we expect to be finished in the near term. We do not however consider
it the end of road for Solidity, but rather as a foundation for future expansion. While the
following list is tentative, non exhaustive, and subject to significant change, these are some of
the features that we currently consider interesting for future post-core iterations of the language:

- Linear types: Linearity is a deeply powerful primitive. We consider its resource semantics to be
  particularly well suited to enforce the kind of accounting invariants that are often of interest
  for systems written in Solidity. Linearity alone is powerful enough to be able construct advanced
  features like object capabilities, effect systems, and session types,
  significantly expanding the scope and complexity of invariants that can be guaranteed by the type system.
  Linearity can be used to help guarantee the safe usage of memory, allowing users to
  optimize without fear, and giving the compiler itself the context it needs to be able to safely
  eliminate unnecessary allocations and make more optimal usage of memory.

- Macros: Since a great deal of the compilation stack for Core Solidity is already designed around simple
  macro like syntactic transformation passes, a natural extension to the language would be to
  implement a user facing macro system, and reimplement these desugaring passes as in language macro
  transformations. This would give a similar level of expressive power and flexibility as languages
  with cutting edge macro systems like Lean or Racket, allowing library authors to introduce
  arbitrary new syntax and even supporting the construction of entirely new languages on top of SAIL.
  While attractive in many ways, we are also cautious about the potential for misuse such a feature
  would have, and would want to take great care to implement sufficient safeguards against obfuscation
  of malicious code.

- Refinement Types: Refinement types are an intuitive and user friendly way to document and enforce
  program level invariants. We are particularly interested in schemes that implement decidable
  logics (as opposed to full SMT based approaches), which we consider more likely to be usable at
  scale by non experts (although of course with an associated tradeoff in the complexity of properties
  that can be expressed).

- Theorem Proving: Code written in Solidity often manages large amounts of money in a highly
  adversarial environment. Correctness is of the utmost importance. Languages like
  [ATS](https://ats-lang.sourceforge.net/) and [Bedrock 2](https://github.com/mit-plv/bedrock2) have
  shown how the integration of theorem proving with low level systems orientated languages
  can be used to support the production of code that is both correct and maximally resource efficient.
  We are interested in investigating the degree to which the kind of semi-automated reasoning
  available in theorem provers could be integrated directly into the language (likely via an Isabelle
  style embedding of an appropriate logic via the module system).

## Conclusion

Core Solidity represents a foundational re-imagining of the language, designed to equip developers
with a more secure, expressive, and mathematically sound toolkit for the next generation of smart
contracts. We invite you to join the discussions and share your perspective. Your input is crucial
in helping us prioritize development and shape the future of the language, and comments are very
welcome in the [feedback thread](TODO: LINK TO THREAD) for this post on our forum.
