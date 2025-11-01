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

In addition to growing and expanding the language, we will also be removing or reworking some
existing features. We are already certain that we will be removing inheritance entirely. Additional
changes are less certain, but we are considering potentially replacing or reworking problematic features
like try/catch, libraries, or function pointers.

We currently have a working prototype for Core Solidity. All examples in the post typecheck and
produce executable code. The syntax is far from final, and extensive changes should be expected
before release. Much of the core type theory is stable, but we want to add at least compile time
evaluation and modules before we will consider the type system finalized. Extensive work remains to
build out the standard library and reach feature parity with Classic Solidity.

In this post we will present our current progress and give you some idea of our vision for the
future of the language. It is important to understand that the version presented here is not final,
changes can and will be made before release. We are presenting our current progress now in order to
receive feedback and to allow for course correction if necessary.

## A Note on Syntax

Most of the work to date has been focused on the design and implementation of the type system, and
associated code generation pipeline down to Yul. In order to avoid getting bogged down in
bikeshedding around syntax, and with the desire to validate our core ideas with a working
implementation as soon as possible, we moved ahead with a provisional syntax. You can expect
extensive changes before release. Our current intention is to eventually match the syntax of Classic
Solidity as much as possible. For any new syntax, you should expect that the final version will feel
closer to languages like TypeScript or Rust than it does right now.

We made the decision to share before we have a finalized syntax because we are both excited to share
our progress, and wish to gather feedback sooner rather than later.

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
production implementations: assembly will remain a core primitive with an important new feature:
we will support calling functions from the high level language in assembly directly. Users will
be able to disable the built in abstractions (e.g. contract dispatch generation, ABI decoding,
default storage layout generation), following the "pay for what you use" philosophy of languages
like Rust and C++.

Aside from the removal of inheritance, we expect to be able to support the majority of existing
language features without breakage.

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
ways for different types. Type classes are the standard way of combining overloading and
parametric polymorphism (generics) in a systematic manner. Type classes are similar to
Rust traits: they provide signatures for the functions which will be implemented, for
distinct types, in instance definitions. In this sense, instance declarations are similar
to `impl` in Rust.

A type class definition declares the class name, its arguments and member functions type
signatures. As example, let's consider the following definition of a class for the ABI
encoding of types:

```
forall self . class self:ABIEncode {
    function encodeInto(x:self, basePtr:word, offset:word, tail:word) -> word;
}
```

Function `encodeInto` encodes a value of type `self` into a memory region starting at
`basePtr`. Argument `offset` denotes the position of the first empty byte of the head
and `tail` which gives the position of the first empty byte of the tail. The
implementation of encoding for a specific type is done in an instance declaration.
As an example, the following instance definition implements the ABI encoding for
`uint256` type, which is represented as an algebraic type that holds a `word` value.

```
data uint256 = uint256(word);
instance uint256:ABIEncode {
    function encodeInto(x:uint256, basePtr:word, offset:word, tail:word) -> word {
        let repx : word = Typedef.rep(x);
        assembly { mstore(add(basePtr, offset), repx) }
        return tail;
    }
}
```
Type classes and instances can be used to avoid repetitive definitions.
As an example, consider the [console](https://github.com/foundry-rs/forge-std/blob/master/src/console.sol) library
present in the foundry framework. Looking the code, we can see
that there are several one argument functions which only call the ABI
encoding for the value being logged and then perform a static call to
the console address. All these one argument functions definitions can
be reduced to following single function, which uses the standard library
machinery for ABI encoding of values.

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
) public pure returns (uint256, uint256) {
    uint256 totalDeposits = depositAmount;
    uint256 totalWithdrawals = withdrawalAmount + feeAmount;
    uint256 balanceAfterDeposit = initialBalance + totalDeposits;
    uint256 netChange = totalDeposits - totalWithdrawals;
    uint256 finalBalance = balanceAfterDeposit - totalWithdrawals;
    uint256 totalChanges = totalDeposits + totalWithdrawals;
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

Type inference can avoid the need of coercions on array literal expressions,
which are necessary in Classic Solidity. As an example, consider the following
definition in Classic:

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

Desugaring is the process of translating high-level language constructs into
a semantically equivalent form using language's more primitive features.
Some of the Core Solidity new high-level constructs will be translated
into a intermediate language called SAIL - Solidity Algebraic Intermediate
Language.


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
