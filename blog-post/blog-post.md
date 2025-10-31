# Introducing Core Solidity

Solidity is the most widely used smart contract language. It is robust, trustworthy, and today
secures hundreds of billions of dollars of value. We are proud of this success, and its track
record of secure code generation. Users of Solidity will however be keenly aware of some of its
limitations. The type system often lacks the expressiveness to produce reusable library code or
enforce core safety properties. The language has very limited capabilities around compile time
evaluation. Many features are implemented in an inconsistent manner, or do not always work as
expected.

Fixing these limitations within the current implementation has proven difficult. Upgrades must be
made in a somewhat ad-hoc manner, and each new feature addition makes reasoning about the
correctness of subsequent features more difficult. We did not feel confident that we would be able
to safely extend the language in this way to add the kind of features that our users were asking
for, and that we feel are required to keep up with the ever increasing scale of systems that are
being developed in Solidity.

Core Solidity is our solution. It is a rebuild of the Solidity type system and compiler front/middle
end that will:

- introduce powerful new features (generics, traits, algebraic datatypes, pattern matching, comptime)
- provide a strong foundation for compiler correctness as we continue to extend the language in the future
- empower library authors and support a community-driven process for language evolution
- expand the capabilities of verification and analysis tooling

In this post we will present our current progress and give you some idea of our vision for the
future of the language. It is important to understand that the version presented here is not final,
changes can and will be made before release. We are presenting our current progress now in order to
receive feedback and to allow for course correction if necessary.

As a summary of the current state of things: We have a working prototype for the new language
version. All examples in the post typecheck and produce executable code. The syntax is far
from final, and extensive changes should be expected before release. Much of the core type theory is
stable, but we want to add at least compile time evaluation and modules.

## A Note on Syntax

Most of the work to date has been focused on the design and implementation of the type system, and
associated code generation pipeline down to Yul. In order to avoid getting bogged down in
bikeshedding around syntax, and with the desire to validate our core ideas with a working
implementation as soon as possible, we moved ahead with a provisional syntax. You can expect
extensive syntax changes before release. Our current intention is to match the syntax of Classic
Solidity as much as possible. For any new syntax, you should expect that the final version will feel
closer to more familiar languages like TypeScript or Rust than it does right now.

We made the decision to share before we have a finalized syntax because we are both excited to
share our progress, and wish to gather feedback sooner rather than later.

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
production implementations: assembly will remain a core primitive, and we will support calling
functions from the high level language in assembly directly. Users will be able to disable the built
in abstractions (e.g. contract dispatch generation, ABI decoding, default storage layout generation)
following the "pay for what you use" philosophy of languages like Rust and C++.

We will be removing inheritance, aside from that we expect to be able to support the majority of
existing language features without breakage.

### Generics and type classes

Core Solidity introduces two new exciting abstraction mechanisms: generics and
type classes.

Generics enable parametric polymorphism through type parameters, which allows the
implementation of algorithms and data structures that operate uniformly across
all types. As an example, we could define a polymorphic `identity` function:

```
forall T . function identity(x : T) -> T {
    return x;
}
```

While generic functions are interesting, most interesting operations are not defined
for all types. Overloading allows the definition of code which can operate in distinct
ways at different types. Type classes are the standard way of combining overloading and
parametric polymorphism (generics) in a systematic manner. Type classes are similar to
Rust traits: they provide signatures for the functions which will be implemented, for
distinct types, in instance definitions. In this sense, instance declarations are similar
to `impl` in Rust.

A type class definition declares the class name, its arguments and member functions type
signatures. As an example, let's consider the task of defining addition over different types:

```
forall T . class T : Sum {
    function sum (x : T, y : T) -> T;
}
```

Implementations of member functions for different types are provided by instance
definitions. As an example, let's consider the implementation of `Sum` for
`word` type which, internally, uses Yul assembly to perform addition.

```
instance word : Sum {
    function sum (x : word, y : word) -> word {
        let res : word ;
        assembly {
            res := add(x,y);
        }
        return res;
    }
}
```
The type `word` correspond to `uint256` in Classic Solidity. Another
more interesting example is the instance for `uint128`, which is
defined as follows:

```
data uint128 = uint128(word);

instance uint128 : Sum {
    function sum(x : uint128, y : uint128) -> uint128 {
        let res : word;
        match x, y {
        | uint128(n), uint128(m) =>
            assembly {
                res := add(n,m);
                // checks for overflow here
            }
        }
        return uint128(res);
    }
}
```

We start by defining a new type for `uint128` which will, internally,
hold a `word` value. The implementation of `sum` for `uint128`
uses pattern matching to extract the internal `word` values and
performs the addition in a Yul block, which reverts if the result
is not a valid unsigned 128 bits integer.

Instances can also be defined over polymorphic types. As an example,
consider the following implementation of `sum` for polymorphic pairs.

```
forall T1 T2 . T1 : Sum, T2 : Sum => instance (T1,T2) : Sum {
    function sum (p1 : (T1,T2), p2 : (T1,T2)) -> (T1, T2) {
        match p1, p2 {
        | (x1, y1), (x2, y2) =>
            return (Sum.sum(x1,x2), Sum.sum(y1,y2));
        }
    }
}
```
The previous definition shows that, whenever we have instances of
class `Sum` for types `T1` and `T2`, then we can also use function
`Sum.sum` on pairs of such types. The reader should note that the
last two examples use **pattern matching** to extract components
of user defined algebraic types, which are
another feature that will be part of Core Solidity.

Type classes and instances can be used to avoid repetitive definitions.
As an example, consider the [console](https://github.com/foundry-rs/forge-std/blob/master/src/console.sol) library
present in the foundry framework. Looking the code, we can see
that there are several one argument functions which only call the ABI
encoding for the value being logged and then perform a static call to
the console address. All these definitions can be reduced to following
single function, which uses the standard library machinery for ABI
encoding of values.

```
forall ty . ty : ABIAttribs, ty : ABIEncode => function log(val : ty) {
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

Another short possible implementation which relies on EVM `log1` instruction,
instead of Foundry console address, would be:

```
forall t . t : Typedef(word) => function log1(v : t, topic : word) -> () {
  let w : word = Typedef.rep(v);
  assembly {
    mstore(0,w)
    log1(0,32,topic)
  }
}
```

This version uses standard library class `Typedef`, which provides functions for
the conversion between `word` and types which are instance of this class.

### Algebraic data types and pattern matching

Algebraic Data Types provide a way of data modeling using sum types
(disjoint unions) and product types (structural records). This enables the construction of
precise types where invalid states are **unrepresentable** by design, thereby enhancing program
correctness through the type system itself. As an example, consider the following type
definition for an auction state.

```
data AuctionState =
    NotStarted (word)
  | Active (word, address)
  | Ended (word, address)
  | Cancelled;

```
The type has three constructors: `NotStarted` specifies that the auction has not started yet and
stores its reserved price, `Active` denotes that the auction has begun and it stores the
current highest bid and the address that made such bid, `Ended` represents that the auction has
finished with success and it holds the highest bid and the winner address and constructor,
`Cancelled` is used when the auction has been cancelled.

Using algebraic data types, we can define functions by pattern matching. As an example,
consider function `processAuction` which tries to update the action state based on current
state and `msg.value`.  Pattern matching provides structural decomposition of data types through
case analysis. This ensures all possible variants are handled explicitly, eliminating
partial function hazards and providing formal guarantees of match completeness.

```
function processAuction(state) {
    match state {
    | NotStarted(reserve) =>
        require(msg.value >= reserve);
        return Active(msg.value, msg.sender);
    | Active(currentBid, bidder) =>
        require(msg.value > currentBid);
        transferFunds(bidder, currentBid);
        return Active(msg.value, msg.sender);
    | Cancelled =>
        revert();
    |   _ =>
        return state;
    }
}
```

### High-order functions

Functions possess first-class status within the type system, enabling their use
as parameters, return values, and assignable entities. This facilitates the
implementation of higher-order functions and functional composition patterns,
enhancing language expressivity.

As an example of a high-order function, let's consider `map_pair`
that takes two functions and applies them to the corresponding
components of a pair:

```
forall T1 T2 U1 U2 . function map_pair (pair : (T1, T2), f1 : (T1) -> U1, f2: (T2) -> U2) -> (T2,U2) {
    match pair {
    | (t1,t2) =>
        return (f1(t1), f2(t2));
    }
}
```


### Type inference

Core Solidity uses type inference algorithm to reduce syntactic verbosity while
maintaining the strong static typing guarantees. The type inference occurs
during compilation and provides complete type safety without explicit annotations. As an example, consider the following code snippet in Classic Solidity:

```
function adjustBalance(
    uint256 initialBalance,
    uint256 depositAmount,
    uint256 withdrawalAmount,
    uint256 feeAmount
) public pure returns (uint256 finalBalance, uint256 totalChanges) {
    uint256 totalDeposits = depositAmount;
    uint256 totalWithdrawals = withdrawalAmount + feeAmount;
    uint256 balanceAfterDeposit = initialBalance + totalDeposits;
    uint256 netChange = totalDeposits - totalWithdrawals;
    finalBalance = balanceAfterDeposit - totalWithdrawals;
    totalChanges = totalDeposits + totalWithdrawals;
    return (finalBalance, totalChanges);
}
```

All local variables and function arguments and result needs to specify
their type. In Core Solidity, thanks to type inference, we could
completely omit type annotations, while keeping the guarantees about
the code type safety.

```
function adjustBalance(initialBalance, depositAmount, withdrawalAmount,feeAmount) {
    let totalDeposits = depositAmount;
    let totalWithdrawals = withdrawalAmount + feeAmount;
    let balanceAfterDeposit = initialBalance + totalDeposits;
    let netChange = totalDeposits - totalWithdrawals;
    let finalBalance = balanceAfterDeposit - totalWithdrawals;
    let totalChanges = totalDeposits + totalWithdrawals;
    return (finalBalance, totalChanges);
}
```

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
        payable(payment.to).transfer(payment.amount);
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

We want to make sure the needs of the community are met here. Please feel free to reach out to use
on Github / The Solidity Forum, or our Matrix channel if you have concrete use cases in mind for
this feature.

We will publish more once we have a concrete design / prototype implementation.

## Current status

Core Solidity prototype is being actively developed by Argot's Programming Languages
Research team and it currently supports the following features:

- Definition of algebraic data types, pattern matching, type classes and polymorphic functions.

- Data Locations represented as types: Data locations (e.g., storage, memory)
  are now just standard library types. This enables the creation of composite
  types with mixed locations, such as `Broker` type which holds references to storage
  arrays and memory data, as presented in the next code piece:

```
data Broker = Broker (memory(word), storage(array(address, word)))
```

- Classic Solidity compatible ABI encoding / decoding, mappings and contract constructors.

Using these features, we can implement a simple [ERC20 contract in Core Solidity](erc20.sol).

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
multiple incompatible language versions in the same project (TODO: check and confirm the state of
things in foundry / hardhat).

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
problematic or unsatisfying, in particular: Libraries, Try/Catch, and Function Pointers. Input or
feedback on the ways in which these features do or do not work for you is interesting and very
welcome. We will publish more on this area once we have concrete plans and implementations to share.


## What's next?

The evolution of Solidity has reached a crucial moment. While the language's
established features have successfully powered the ecosystem, certain patterns have
revealed limitations in scalability, security, and clarity. This blog post outlines
Argot Collective's vision for Core Solidity, a deliberate re-design of the language's
foundation. This initiative aims to replace legacy mechanisms with more robust,
composable, and community-driven primitives, ensuring the language remains a secure
and efficient basis for the next generation of smart contracts.

Our next steps will involve:

- Type aliases and definitions: Currently the Core Solidity prototype do not support
  the definition of type aliases and structs.

- Inheritance replacement: While a basic building block of Classic Solidity, inheritance has
  often failed as a clean code reuse mechanism. Type classes are our intended, more robust and
  composable replacement in the new language.

- No more `try`/`catch`: The `try`/`catch` construct has always been problematic in Solidity.
  Instead of fixing its deficiencies, Core Solidity will rely on pattern matching against error
  objects to provide explicit error handling that address all the corner cases `try`/`catch`
  misses.

- A better high-level `delegatecall` mechanism: While not final, traditional libraries are likely
  to be replaced. Free functions and modules can adequately replace internal library functions. We
  are designing a new, first-class mechanism for splitting contracts and connecting pieces via
  `delegatecall` to replace the role of external libraries.

- A community-driven standard library: An overarching goal of Core Solidity is to have a
  simple, flexible language core, with much of the current built-in functionality defined
  in-language as part of the standard library. Currently, we have a prototype standard library
  which has type class-based constructions for the representation of mappings, ABI encoding/decoding
  and dispatch. Features like arbitrary arrays and slices over them will be implemented as
  part of this prototype library.

- Compile time evaluation: Inspired by Zig, we plan to include compile-time code
  evaluation in Core Solidity. Details on how such feature will work are being
  discussed by Argot Collective Programming Languages research team.

- A formal specification of the language: Having a unified language specification will
  avoid language fragmentation by diverging implementations. Using a unified specification,
  compilers can then compete on optimization quality and alternative standard library
  implementations.

## Conclusion

Core Solidity represents a foundational re-imagining of the language,
designed to equip developers with a more secure, expressive, and mathematically
sound toolkit for the next generation of smart contracts. The path forward,
however, will be designed by the community that uses it. We invite you to join
the discussions and share your perspective. Your input is crucial in helping us
prioritize this exciting roadmap.
