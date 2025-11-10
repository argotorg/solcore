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
feel are required to keep up with the ever increasing scale of systems that are being developed in
Solidity.

Core Solidity is our solution. It is a rebuild of the Solidity type system and compiler front/middle end that will:

- introduce powerful new features (generics, traits, algebraic datatypes, pattern matching, compile-time constant evaluation)
- provide a strong foundation for compiler correctness as we continue to extend the language in the future
- empower library authors and support a community-driven process for language evolution
- expand the capabilities of verification and analysis tooling

In addition to growing and expanding the language, we will also be removing or reworking some
existing features. We are already certain that we will be removing inheritance entirely. Additional
changes are less certain, but we are considering potentially replacing or reworking problematic features
like try/catch, libraries, and function pointers.

We currently have a working prototype for Core Solidity. All examples in the post typecheck and
produce executable code. The syntax is far from final, and extensive changes should be expected
before release. Much of the core type theory is stable, but we want to add at least compile time
evaluation and modules before we will consider the type system finalized. Extensive work remains to
build out the standard library and reach feature parity with Classic Solidity.

## A Note on Syntax

Most of the work to date has been focused on the design and implementation of the type system, and
associated code generation pipeline down to Yul. In order to avoid getting bogged down in
bikeshedding around syntax, and with the desire to validate our core ideas with a working
implementation as soon as possible, we moved ahead with a provisional syntax. You can expect
extensive changes before release. Our current intention is to eventually match the syntax of Classic
Solidity as much as possible. For any new syntax, you should expect that the final version will feel
closer to languages like TypeScript or Rust than it does right now.

## New Language Features

Core Solidity takes ideas from pure functional programming languages (e.g. Haskell, Lean), as well
as modern systems languages (e.g. Rust, Zig). The core new features that we plan to add are:

- Generics / parametric polymorphism
- Traits / typeclasses
- Algebraic datatypes (a.k.a sum / product types) and pattern matching
- Higher order functions
- Type inference
- Compile time evaluation

We think that taken together these core primitives will enable developers to produce stronger
abstractions, write more modular and reusable code, leverage the type system to enforce core
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
variant. Product types combine multiple values into structured tuples. The combination of these two
primitives allows for the construction of precise types that can make invalid states completely
**unrepresentable**, allowing the type system to guarantee core program invariants entirely at
compile time.

Lets start with a very simple type:

```solidity
data Bool = True | False
```

Types are sets of values. The left hand side of the above statement defines the name of a new type
(`Bool`), and the right hand side defines the set of allowed values of the `Bool` type (`True` or
`False`). The `|` lets us introduce new alternative value constructors.

Alternatives can also hold values. This lets us implement the same kind of patterns as [User Defined
Value Types](https://docs.soliditylang.org/en/latest/types.html#user-defined-value-types) would in
Classic Solidity. We can consider a simple 18 decimal fixed point (a `wad`):

```solidity
data wad = wad(uint256)
```

The `wad` type has a single value constructor: `wad` (type names and value constructors live in
separate namespaces and so can share names) that holds a `uint256` as it's underlying
representation. Simple wrapper types like this will be erased by the compiler during the translation
into yul, meaning that `wad` has the exact same runtime representation as a `uint256`.

Now we can define a type safe fixed point multiplication routine. We will need to extract the
underlying `uint256`, manipulate it, and then wrap it up again in a new `wad` constructor. To unwrap
we will use pattern matching. Pattern matching is a control flow mechanism that lets you destructure
and inspect data by shape. Instead of nested nested if-else chains, we can write declarative
expressions that exhaustively consider all possible values of the matched type.

For our simple `wad` examplle, we won't have any branches, but we can still use pattern matching to
destructure our inputs and extract their underlying representation:

```solidity
let WAD = 10 ** 18;

funtion wmul(lhs : wad, rhs : wad) -> wad {
    match (lhs, rhs) {
    | (wad(l), wad(r) => return wad((l * r) / WAD);
    }
}
```

Lets look at a more complete example. Consider the following type definition for an auction state:

```solidity
data AuctionState =
    NotStarted(uint256)
  | Active(uint256, address)
  | Ended(uint256, address)
  | Cancelled;
```

`AuctionState` has four alternative value constructors: `NotStarted` specifies that the auction has not
started yet and stores its reserved price, `Active` denotes that the auction has begun and it stores
the current highest bid and the address that made such bid, `Ended` represents that the auction has
finished with success and it holds the highest bid and the winner address and constructor,
`Cancelled` is used when the auction has been cancelled.

Now we can define a `processAuction` function that transitions the state based on the current state
and `msg.value`. The `match` statement lets us perform an exhaustive case analysis over each
possible alternative state. Exhaustiveness can be enforced by the compiler, ensuring that we do not
accidentally miss or fail to consider a critical state:

```solidity
function processAuction(state) {
    match state {
    | NotStarted(reserve) =>
        require(msg.value >= reserve);
        return Active(msg.value, msg.sender);
    | Active(currentBid, bidder) =>
        require(msg.value > currcompelingentBid);
        transferFunds(bidder, currentBid);
        return Active(msg.value, msg.sender);
    | Ended(_, _) =>
        return state;
    | Cancelled =>
        revert();
    }
}
```

Sometimes we want to take the same action for many states, or handle certain subcases of a state in
special ways. Pattern matching is expressive enough to let us do exactly that. Let's continue the
auction example by defining a function that checks refund eligibility based on auction state.

```solidity
data Refund
  = RefundAvailable
  | NoRefund(memory(string));

function canRefund(state) {
    match (state) {
    | Ended(winner, 0) =>
            return NoRefund("Auction ended with no bids. No winner.");
    | Ended(winner, finalBid):
            return NoRefund("Auction was successful. No refunds.");
    | Active(highestBid, highestBidder):
            return NoRefund("Auction is still active. No refunds available.");
    | _ =>
            return RefundAvailable;
    }
}
```

The `Refund` type explicitly models the two possible outcomes: either a refund is available,
or it's not (with a reason). The `canRefund` function uses pattern matching to inspect the
auction state, handling specific cases first: if the auction ended with a zero bid, it
provides a tailored message; for other successful or active states, it returns appropriate
refusal reasons. The wildcard pattern, `_`, acts as a default option, granting refunds for
all remaining states (like `NotStarted` or `Failed`), ensuring that every possible
state is handled exactly once.

### Generics and type classes

Core Solidity introduces two new mechanisms for code sharing and polymorphism: generics and
type classes.

Generics enable us to write functions and data structures that can operate in a uniform way across
all types. As an example, we can define a polymorphic `identity` function:

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

Generics are powerful, but by themselves quite limited: most interesting operations are not defined
for all types. Type classes (a.k.a Traits if you are a rust programmer) are the solution: they give
us the tools we need to write functions that are polymorphic over a restricted subset of types.

A type class is simply an interface specification. Consider the following defition of a class of
types that can be multiplied:

```solidity
forall T . class T:Mul {
    function mul(lhs : T, rhs : T);
}
```

Instead of the concrete `wmul` function that we dfined above for our `wad` fixed point type, it
would be more idiomatic to defined an instance (a.k.a `impl`) of the `Mul` type class for `wad`
(giving us a uniform syntax for multiplication across all types, and allowing us to use our `wad`
type in functions that are generic over any instance of the `wad` type class):

```solidity
instance wad:Mul {
    function mul(lhs : wad, rhs : wad) {
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
pattern matching everytime we want to unwrap a type. The instance for `wad` would look like this:

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
multi-parameter typeclasses.

For a real world example of how generics and type class constraints can be used to eliminate
boilerplate or repetitive code, compare the combinatorial explosion of overloads required for the
[`console.log` implementation](https://github.com/foundry-rs/forge-std/blob/master/src/console.sol)
in `forge-std` to the following generic Core Solidity function that covers the functionality of all
the single argument overloads from the original library:

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

### Higher-order and anonymous functions

Functions possess first-class status within the type system, enabling their use
as parameters, return values, and assignable entities. This facilitates the
implementation of higher-order functions and functional composition patterns,
enhancing language expressivity.

As an example, consider the following which implements a custom ABI encoding
of a triple of booleans into a single `word` value:

```
forall ret . function unpack_bools(bools : word, fn : (bool, bool, bool) -> ret) -> ret {
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

In order to call `unpack_bools`, we must provide a function that handles the triple of
boolean values to produce some result. In following piece, we use `unpack_bools` and
a anonymous function that takes a triple of booleans and conjunct them to produce a
boolean which is then stored in the `res` variable.

```
let res : bool
  = unpack_bools (bools, lam (p){
        match p {
        |   (x,y,z) => return x && y && z;
        }});
```

Anonymous functions enable writing concise, focused logic directly at the call site,
improving code readability and reducing boilerplate.

### Type inference

Core Solidity uses type inference algorithm to reduce syntactic verbosity while
maintaining the strong static typing guarantees. The type inference occurs
during compilation and provides complete type safety without explicit annotations,
which are required in Classic Solidity. As an example, consider the following
definition in Classic:

```
bytes32 y;
uint x = uint(y);
```

That demands a type annotation on `x`'s definition even when it has an explicit
cast to `uint` type. In Core Solidity we could simply define `x` without its type:

```
bytes32 y;
let x = uint(y);
```

Another situation which have unnecessary annotation involves `struct` value
constructors. Consider the following `struct` definition which could be
part of some lending protocol:

```
struct LendingPosition {
    uint256 collateralDeposited;
    uint256 debtIssued;
    uint256 lastUpdated;
}
```

Creating a variable to represent an initial position would be as follows in
Classic:

```
LendingPosition emptyPosition = LendingPosition(0, 0, block.timestamp);
```

Since we are using the `LendingPosition` constructor, the compiler can infer
the correct type. The same definition for `emptyPosition` in Core Solidity would be:

```
let emptyPosition = LendingPosition(0, 0, block.timestamp);
```

A similar situation happens with array literals. Consider the following simple
array definition in Classic Solidity:

```
uint[3] memory a = [1, 2, 3];
```

The declaration is rejected by the compiler, which returns the following error message:

```
Error: Type uint8[3] memory is not implicitly convertible to expected type uint256[3] memory.
```

The reason of this error is the fact that Classic Solidity uses as the base type of the array
the type of the first expression on the list and it tries to implicit convert all other elements
to this type. The compiler emits a type error when such coercion is not possible. In order to
the previous definition be accepted, we need to add a type coercion to the array first element
as follows:

```
uint[3] memory a = [uint(1), 2, 3];
```

More about array literals in Classic Solidity can be found in the [language documentation.](https://docs.soliditylang.org/en/latest/types.html#array-literals)
Core Solidity will solve this problem by allowing **overloaded literals**, a feature present in
Lean and Haskell, which allow numeric literals to be interpreted as values of any type that
implements the `Num` typeclass, rather than being fixed to a single concrete type.
Thanks to overloaded numeric literals, the expression:
```
uint[3] memory a = [1, 2, 3];
```
would be accepted directly, without the need of an explicit type coercion on the array
first element, if the type `uint` is an instance of the `Num`.


## Extended example: Classic Solidity vs Core Solidity

Now, let's consider an extended example: a contract which implements a unified payment
processor that handles three different token standards. The complete Classic Solidity
implementation for this simple contract can be found [here.](PaymentHandler.sol)
The code starts by defining a `Payment` struct that attempts to represent all three token standards,
using the `paymentType` field as a runtime discriminator to determine which fields are relevant
for each case.

```
enum PaymentType { NATIVE, ERC20, ERC721 }

struct Payment {
    PaymentType paymentType;  // Runtime type tag
    address token;           // Used for ERC20/ERC721, must be zero for Native
    address from;            // Sender address
    address to;              // Receiver address
    uint256 amount;          // Used for Native/ERC20, must be 1 for ERC721
    uint256 tokenId;         // Used for ERC721, must be zero for others
}
```

Next, functions `processPayment` and `calculateFee` uses explicit `if`/`else` chains
to manually check the `paymentType` and route to the appropriate handling logic.
Also, in order to avoid invalid states, the code uses `require` to ensure that
each case is dealing with a proper payment state value.

```
function processPayment(Payment calldata payment) external {
    if (payment.paymentType == PaymentType.NATIVE) {
        require(payment.token == address(0), "Native: no token");
        require(payment.amount > 0, "Native: amount required");
        require(payment.tokenId == 0, "Native: no tokenId");
        payable(payment.to).call{value: payment.amount}("");
  } else if (payment.paymentType == PaymentType.ERC20) {
        require(payment.token != address(0), "ERC20: token required");
        require(payment.amount > 0, "ERC20: amount required");
        require(payment.tokenId == 0, "ERC20: no tokenId");
        IERC20(payment.token).transferFrom(payment.from, payment.to, payment.amount);
  } else if (payment.paymentType == PaymentType.ERC721) {
        require(payment.token != address(0), "ERC721: token required");
        require(payment.amount == 1, "ERC721: amount must be 1");
        require(payment.tokenId > 0, "ERC721: tokenId required");
        IERC721(payment.token).transferFrom(payment.from, payment.to, payment.tokenId);
  }
}
```

Now, let's turn our attention to the encoding of this example in Core Solidity.
The complete encoding of this example can be found [here](payment.sol).

First, we start by defining an algebraic data type which represents each payment
type by a separate data constructor with precisely the fields required for that
specific standard. This makes invalid states **unrepresentable** by design. As an
example, you cannot create a `Native` payment with a token field or an `ERC721`
payment with an amount field.

```
data tokenid = tokenid(word);

data Payment =
    Native(address, word)
  | ERC20(address, address, address, word)
  | ERC721(address, address, address, tokenid);
```

The implementation of functions `processPayment` and `calculateFee` can be made
much simpler by using pattern matching:

```
function processPayment(payment : Payment) {
    match payment {
    | Native(to,amount) =>
        transfer(to,amount);
    | ERC20(token,from,to,amount) =>
        transferFromERC20(from, to, amount);
    | ERC721(token,from,to,tokenId) =>
        transferFromERC721(from, to, tokenId);
    }
}
```

Another effect of the use of pattern matching / algebraic data types is that,
since it is not possible to represent invalid states, no runtime validations
using `require` is necessary, since they are enforced by Core Solidity type system.

In this short example, we can see how Core Solidity gives the developer the tools
to write shorter and safer code. By using algebraic data types and pattern matching,
we can avoid the manipulation of invalid states and the need of runtime validations
using `require`. Also, parametric polymorphism (a.k.a. generics) and type classes
open up new possibilities for the development of safer libraries for the modular
development of smart-contracts.

### Compile Time Evaluation

While not yet implemented, we are planning to add a robust system for compile time evaluation. We
expect to support at least the compile time evaluation of pure expressions that do not require
access to memory. Some key design questions that remain open relate to the degree to which we will
support access to memory at compile time, the need for compile time specific primitives (e.g. string
concatenation / hashing), as well as the need for reflection.

We want to make sure the needs of the community are met here. Please feel free to reach out
The Solidity Forum, or our Matrix channel if you have concrete use cases in mind for
this feature.

We will publish more once we have a concrete design / prototype implementation.

## SAIL, Desugaring and the Standard Library

In addition to expanding the surface language, the transition to Core Solidity will also introduce a
new mid level IR to the compiler: SAIL (Solidity Algebraic Intermediate Language). This is the
"Core" in Core Solidity. It's the most minimal language we could conceive of that would let us
express the full range of high-level language constructs found in Classic Solidity. It consists of
the following primitive constructs:

- Functions
- Contracts
- Assembly blocks
- Simple variable introduction and assignment
- A short circuiting if-then-else expression
- Algebraic datatypes & pattern matching
- Typeclasses
- Generics

It has a single builtin type (`word`) that has the same range of values as a Classic Solidity
`bytes32` or `uint256`, and can semantically can be viewed as "an item on stack" or "a yul
variable". Contracts in SAIL are very low level (essentially just a runtime entrypoint and initcode
entrypoint).

All other high level language features and datatypes will be constructed as a combination of
standard library definitions and desugaring passes (compile time syntactic transformations into SAIL
primitives). This style of language construction is often used in other high assurance domains (e.g.
theorem provers), and we believe it has important benefits for both users of the language and the
safety and security of it's implementation.

SAIL is simple enough that we expect to be able to construct an executable formal semantics for it.
This will be useful in a lot of directions: We will be able to mathematically guarantee certain core
properties of the Core Solidity type system itself. We will have a reference implementation that we
can use for differential fuzzing of the eventual production Core Solidity implementation. We will be
able to formally verify the standard library and implementation of higher level language constructs.
We believe that this will be an essential part of our correctness story as both the language and
the scale of the systems it is used to construct continue to grow.

Library authors will have (almost) the same expressive power as the language designers, and will
have the power to build abstractions that feel built-in to the language itself (a "library-based
language"). It will be possible to define and use alternative standard library implementations, or
disable the standard library completely. With the standard library disabled, it will be possible to
write Core Solidity code with almost the same level of control as low level assembly languages like
Huff, but with a modern, expressive type system, based on a mathematically rigorous foundation.

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

#### `uint256` and addition

To begin we will construct the type `uint256`. In Classic Solidity the definition of this
type and it's associated operations are all built in language constructs. In Core, it can be
entirely defined in language. The following snippet defines a new type (`uint256`), and a single
value constructor (also called `uint256`) that can be used to produce a value of type `uint256` from
a `word`. Note that simple wrapper types like this are zero overhead (i.e. the runtime
representation of a `uint256` is just a `word`).

```solidity
data uint256 = uint256(word);
```

Operations like addition and subtraction can also similarly be defined as typeclasses and instances
of them:

```solidity
forall T . class T:Add {
    function add(lhs : T, rhs : T) -> T;
}

instance uint256:Add {
    function add(lhs : uint256, rhs : uint256) -> uint256 {
        // unwrap the two arguments and extract their underlying words
        match (lhs, rhs) {
            | (uint256(l), uint256(r)
                // dispatch to the Add instance for word, and wrap the result in the `uint256` value constructor
                => return uint256(Add.add(l, r))
        }
    }
}
```

To implement the `+` operator, we can then define a simple desugaring pass that replaces the `+`
operator with calls to `Add.add`.

#### `memory` and `bytes`

Similarly we can build types that represent pointers into the various evm data regions by wrapping a
`word`. Notice that in the following snippet the type parameter on the memory pointer is *phantom*
(i.e. it appears only in the type, but is not mentioned in any of the value constructors). This is a
common idiom in ML family languages like Haskell or Rust that lets us enforce compile-time
constraints without runtime overhead.

```solidity
data memory(T) = memory(word)
```

The `bytes` type in Classic Solidity represents a tightly packed byte array with a size only known
at runtime. It doesn't really make sense to have a `bytes` on stack, so we define it as an empty
type (effectively just a compile time tag with no runtime reference) with no value constructors.
This ensures that we can only ever represent a reference to `bytes` on stack (e.g. `memory(bytes)`).

```solidity
data bytes;
```

#### `Typedef` and weak types

These kind of simple wrapper types are very common, and it is helpful to be able to easily unwrap
them and extract their underlying value without having to pattern match. For this reason we define
the following class:

```solidity
forall T U . class T:Typedef(U) {
    function abs(x:T) -> U;
    function rep(x:U) -> T;
}

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

Note that `U` parameter in the above `Typedef` definition is "weak": its value is uniquely
determined by the value of the `T` parameter. If you are familiar with Haskell or Rust, this is
effectively an associated type (although for any type system nerds reading, we implement it using a
restricted form of functional dependencies). To put it more plainly, we can only implement a single
instance of `Typedef` for `uint256`: the compiler would not allow us to implement both
`uint256:Typedef(word)` and `uint256:Typedef(uint128)`. This restriction makes type inference much
more predictable by sidestepping many of the potential ambiguities inherent to full multi-parameter
typeclasses.

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
    function headSize(ty:Proxy(T)) -> word;
    // whether or not `T` is a fully static type
    function isStatic(ty:Proxy(T)) -> bool;
}

instance uint256:ABIAttribs {
    function headSize(ty : Proxy(uint256)) -> word { return 32; }
    function isStatic(ty : Proxy(uint256)) -> bool { return true; }
}
```

Now another that handles the low level encoding into memory. The class presented here contains some
extraneous details needed for encoding compound and dynamic types that are not be necessary for the
simple `uint256` encoding we are implementing now. We present the full complexity to demonstrate
that we have the machinery required for this harder cases.

```solidity
// types that can be abi encoded
forall T . class T:ABIEncode {
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
// abi encodes an instance of `ty` and returns a pointer to the result
forall T . T:ABIAttribs, T:ABIEncode => function abi_encode(val : T) -> memory(bytes) {
    let free = get_free_memory();
    let headSize = ABIAttribs.headSize(Proxy : Proxy(ty));
    let tail = ABIEncode.encodeInto(val, free, 0, Add.add(free, headSize));
    set_free_memory(tail);
    return memory(free);
}
```

## Compatibility and Interoperability

Introducing such a major revision to any programming language is challenging. While a certain degree
of breakage is inevitable (and even desired), we want to make the transition as smooth as possible
and avoid a split in the language.

We intend to keep the syntax breakage as small as possible, and expect to make significant
alterations to the current prototype to bring syntax much more in line with Classic Solidity.

As with previous breaking upgrades to Solidity, ABI compatibility will be maintained between
versions, allowing individual contracts written in incompatible versions to interoperate and live
side by side in the same project (this strategy is also used by Rust with their "Editions" feature).
Thanks to our history of releasing breaking changes, much of the ecosystem tooling already supports
multiple incompatible language versions in the same project.

(TODO: check and confirm the state of things in foundry / hardhat).

We are also investigating the feasibility of deeper interoperability beyond just the contract ABI.
We expect that it will be possible to share at least free functions and interface definitions
between language versions. We will write more concretely on this topic when we have detailed designs
or working prototypes to share.

Avoiding a python2 -> python3 style split is top of mind, and we believe that upgrades should be
manageable and that it will be possible to carry them out in an incremental manner

### Syntax Breakage

The syntax you see in the examples here contains quite some differences from Classic Solidity. As
already mentioned, we expect to bring most of the Core syntax inline with the existing Classic
syntax. After quite some internal discussion and considering user feedback, the current feeling
within the team is that we would prefer to prioritize smoothing the transition over introducing
(subjectively) cleaner syntax.

With that said, some degree of breakage may still be required. Right now we are considering the
following changes:

- Postfix types: Supporting the kind of rich types that Core enables with a prefix syntax is quite
  challenging from a parser implementation point of view. Certain use cases may not be possible at
  all, and those that are may require complex lookahead or backtracking logic. A final decision here
  will not be made until we have completed a deeper analysis of the tradeoffs.
- Ternary operator: Changes may be required here to resolve ambiguities in the language grammar.
- Function type / parameter syntax: The introduction of higher order functions and the more
  functional flavour of Core lends itself much more to the usage of function parameters. While likely not
  strictly necessary for parsing / grammar reasons, we suspect that a simpler syntax here will likely
  be worth the change given the benefits to readability and usability.

### Semantic Breakage and Feature Removal

By far the largest breakage will be the removal of inheritance. Our strong belief is that traits and
generics are a more flexible and powerful system for polymorphism and code sharing, and that
supporting both approaches will not be in the best interests of either the users or developers of
Solidity over the long term. We operate in a high assurance domain where simplicity of semantics and
ease of audit are critical. Keeping the language as simple and focused as possible has deep long
term benefits for ecosystem security.

In addition we are considering changes to some core language features that we currently consider
problematic or unsatisfying, in particular: libraries, try/catch, and function pointers. Input or
feedback on the ways in which these features do or do not work for you is interesting and very
welcome. We will publish more on this area once we have concrete plans and implementations to share.

## The Road to Production

This section outlines our current thinking on achieving production readiness and our strategy for
making such extensive and sweeping changes to the language in a safe way. Please note that this is a
tentative plan, and may be subject to extensive change. We are not in a position where we feel
confident about committing to concrete timelines at this stage. We will provide more detailed
roadmaps and concrete timelines as we get closer to a production implementation.

We have a working prototype implemented in a [separate
repository](https://github.com/argotorg/solcore). All of the features shown in this post work, and
all examples typecheck and produce executable code. We consider the type system to be relatively
stable at this point, and do not expect to be making deep changes to what currently exists. We do
anticipate at least two major additions before we can begin to consider it final: compile time
evaluation and a module system. As discussed already, we will be making extensive changes to the
syntax to bring it more in line with Classic Solidity.

We have a rudimentary standard library implemented, and enough desugaring stages built out to
implement the most fundamental features of Classic Solidity. We can produce ABI compatible
contracts, with dispatch, abi encoding / decoding, and storage access.

There is still significant work remaining at the prototype stage before we can begin to consider a
full production implementation. We want to finalize the type system, flesh out the standard library,
and write enough code to be confident that what we have is sufficient to support the full range of
features that we think are necessary. We need to thoroughly document the typesystem and compiler
internals. We also expect to spend time working with existing power users and library authors to
gather feedback and make any necessary changes.

Once we are feeling confident that the prototype is stable, work will split into two parallel
phases:

1. Production implementation: we will reimplement the typechecker, desugaring and code generation
   passes in a systems language (e.g. Rust, C++, Zig), and integrate it into solc proper. This
   implementation will focus on correctness, performance, and providing the best possible
   diagnostics and error messages.
2. Executable Formal Semantics: we will work to mechanize our existing latex specification in a
   theorem proving environment (likely Lean). We intend to use this definition to prove key
   invariants about the typesystem itself, as well as the standard library. It will also be used
   to build confidence in the production implementation as a differential fuzzing target.

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

- Linear types: We consider linearity as a primitive interesting for both high level protocol
  design and for enhancing the type safety of low level memory management.

- Typesafe Memory Management: Solidity's current allocation strategy is simple, makes use
  after free impossible, and makes allocation cheap at runtime. It does however allow for type
  confusion and makes very inefficient usage of available memory. Since memory expansion is one of the
  more expensive EVM operations, there is clearly significant room for improvement. We are very
  interested in exploring approaches to allocation and memory management that improve on the situation
  here. One possibility we consider attractive is to extend Yul / inline assembly with a fully typed
  memory model. Linearity may also have a role to play. There is a large design space to explore.

- Macros: Since a great deal of the core compilation stack is already designed around simple
  macro like syntax -> syntax transformations, a natural extension to the language would be to
  implement a user facing macro system, and reimplement these desugaring passes as in language macro
  transformations. This would give a similar level of expressive power and flexibility as languages
  with cutting edge macro systems like Lean or Racket. While attractive in many ways, we are also
  cautious about the potential for misuse such a feature would have, and would want to take great care
  to implement sufficient safeguards against obfuscation of malicious code.

- Refinement Types: Refinement types are an intuitive and user friendly way to document and enforce
  complex program level invariants. We are particularly interested in schemes that implement decidable
  logics (as opposed to full SMT based approaches), which we consider more likely to be usable at
  scale by non experts (although of course with an associated tradeoff in the complexity of properties
  that can be expressed).

- Theorem Proving: Code written in Solidity often manages large amounts of money in a highly
  adversarial environment. Correctness is of the utmost importance. Languages like ATS and Idris
  have also show shown how advanced type systems can be used to support the production of code that is
  both correct and maximally resource efficient.

## Conclusion

Core Solidity represents a foundational re-imagining of the language,
designed to equip developers with a more secure, expressive, and mathematically
sound toolkit for the next generation of smart contracts. The path forward,
however, will be designed by the community that uses it. We invite you to join
the discussions and share your perspective. Your input is crucial in helping us
prioritize this exciting roadmap.
