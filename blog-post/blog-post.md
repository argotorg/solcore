# Introducing Core Solidity

Solidity earned its place as the de facto language for smart contracts by 
prioritizing developer experience. Since its initial design, it favored 
high-level abstractions which are well-known by programmers.
While safety was always a goal, it was the history of contract exploits that 
forged it over the years. This focus on security transformed Solidity, making 
it stricter over time. These necessary changes, however, came with consequences. 
The language now exhibits the scars of its accumulated complexity and a type system 
that has become a barrier to its own evolution. Foundational features like 
generics (aka parametric polymorphism) remain out of reach, as integrating them 
into the current architecture is both difficult and dangerously prone to 
introduce critical flaws.

Aiming to break this impasse, the Argot collective decided to embark on a 
fundamental rebuild, which is named Core Solidity: a project to define a new, 
foundational version of the language designed for security, clarity, and 
future growth from the ground up.


## What is Core Solidity?

The design of Core Solidity took inspiration from well established concepts from 
functional programming, with Haskell serving as a primary influence, while 
incorporating rigorous safety guarantees inspired by modern systems programming 
languages such as Rust. Although this architectural direction represents a 
significant evolution, it keeps the domain-specific requirements of smart 
contract development, preserving the established conventions and practical 
utility of Classic Solidity where appropriate.

The language's theoretical foundation comprises several formally-specified 
features that provide enhanced type safety and expressivity. In what follows, we 
provide an overview of such features. However, we warn the reader that the current 
Core Solidity prototype uses a syntax which is similar, but not identical, to 
Classic Solidity. Since our initial concern is to develop the new language type 
system and its semantics, its surface syntax would probably change in the near 
future.

### Generics and Type Classes

Core Solidity will introduce two new exciting abstraction mechanisms: generics and 
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
at all types. Overloading allows the definition of code which can operate in distinct 
ways at different types. Type classes are the standard way of combining overloading and 
parametric polymorphism (generics) in a systematic manner. Type classes are similar to 
Rust traits: they provide signatures for the functions which will be implemented, for 
distinct types, in instance definitions. In this sense, instance declarations are similar 
to `impl` in Rust.

A type class definition declares the class name, its arguments and member functions type 
signature. As an example, let's consider the task of defining addition over different types:

```
forall T . class T : Sum {
  function sum (x : T, y : T) -> T;
```

Implementations for member functions for different types are provided by instance 
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
        if lt(res, n) {
          revert(0,0);
        }
        if gt(res, 0xffffffffffffffffffffffffffffffff) {
          revert(0,0);
        }
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

Instances can be defined over polymorphic types. As an example, consider 
the following implementation of `sum` for polymorphic pairs.

```
forall T1 T2 . T1 : Sum, T2 : Sum => instance (T1,T2) : Sum {
  function sum (p1 : (T1,T2), p2 : (T1,T2)) -> (T1, T2) {
    match p1, p2 {
      (x1,y1), (x2, y2) => 
        return (Sum.sum(x1,x2), Sum.sum(y1,y2));
    }
  }
}
```
The previous definition shows that, whenever we have instances of 
class `Sum` for types `T1` and `T2`, then we can also use function 
`Sum.sum` on pairs of such types. The reader must noticed that
the last two examples use **pattern matching** to extract 
components of user defined algebraic types, which are  
another feature that will be part of the Core Solidity.

### Algebraic data types and pattern matching 

Algebraic Data Types provide a way of modeling data modeling using sum types 
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
The type has three constructors: `NotStarted` specifies that the action have not started yet and 
stores its reserved price, `Active` denotes that the auction has began and it stores the 
current highest bid and the address that made such bid, `Ended` represents that the auction has 
finished with success and it holds the highest bid and the winner address and constructor 
`Cancelled` is used when the auction has been cancelled.

Using algebraic data types, we can define functions by pattern matching. As an example, 
consider function `processAuction` which tries updates the action state based on current 
state and `msg.value`.  Pattern matching provides structural decomposition of data types through 
case analysis. This ensures all possible variants are handled explicitly, eliminating 
partial function hazards and providing formal guarantees of match completeness.

```
function processAuction(state) {
    match state {
    |   NotStarted(reserve) => 
            require(msg.value >= reserve);
            return Active(msg.value, msg.sender);
    |   Active(currentBid, bidder) => 
            require(msg.value > currentBid);
            transferFunds(bidder, currentBid); 
            return Active(msg.value, msg.sender);
    |   Cancelled => {
            revert();
        }
    |   _ => {
            return state;
        }
    }
}
```

### High-order functions

Functions possess first-class status within the type system, enabling their use
as parameters, return values, and assignable entities. This facilitates the
implementation of higher-order functions and functional composition patterns,
enhancing language expressivity. 

As an example of a high-order function signature, let's consider `map` 
which applies a given function to all elements of a memory array

```
forall T U . function map (input : memory(array(T)), transform : (T) -> U) -> memory(array(U))
```

Function `map` receives a memory array formed by elements of type `T` and a function which 
takes a value of type `T` and returns a `U` value. Notation `(T) -> U` represents the 
type of functions which has a `T` argument and a `U` result.

High-order functions allow the elegant encoding of data structure manipulation algorithms 
by encapsulating its traversal logic, making it reusable in different contexts. As an 
example, function `map` could 

### Type inference

Core Solidity uses type inference algorithm to reduce syntactic verbosity while 
maintaining the strong static typing guarantees. The type inference occurs 
during compilation and provides complete type safety without explicit annotations.
As an example, consider the following code snippet that implements a transfer 
function in Classic Solidity:

```
function transferTokens(
    address from,
    address to,
    uint256 amount
) public returns (bool) {
    uint256 fromBalance = balances[from];
    require(fromBalance >= amount, "Insufficient balance");
    
    balances[from] = fromBalance - amount;
    balances[to] = balances[to] + amount;
    
    uint256 newFromBalance = balances[from];
    uint256 newToBalance = balances[to];
    
    bool transferSuccess = (newFromBalance == fromBalance - amount) && 
                           (newToBalance == balances[to] + amount);
    
    return transferSuccess;
}
```

All local variables and function arguments and result needs to specify 
its type. In Core Solidity, thanks to type inference, we could have
completely omit type annotations, while keeping the guarantees about 
the code type safety.

```
function transferTokens(from, to, amount) {
    fromBalance = balances[from];
    toBalance = balances[to]
    require(fromBalance >= amount);
    
    balances[from] = fromBalance - amount;
    balances[to] = balances[to] + amount;
    
    newFromBalance = balances[from];
    newToBalance = balances[to];
    
    transferSuccess = (newFromBalance == fromBalance - amount) && 
                      (newToBalance == toBalance + amount);
    return transferSuccess;
}
```

## Extended example: Classic Solidity vs Core Solidity 

Now, let's consider an extended example: a contract which implements a unified payment 
processor that handles three different token standards. The complete Classic Solidity 
implementation for this simple contract can be found [here.](https://gist.github.com/rodrigogribeiro/7cee270702987fe123921fdbbc2f12ff)
The code starts by defining a `Payment` struct attempts to represent all three token standards, 
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

Next, functions `processPayment` and `calculateFee` uses explicit if/else chains 
to manually check the `paymentType` and route to the appropriate handling logic.
Also, in order to avoid invalid states, the code use `require` to ensure that 
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
The complete encoding of this example can be found [here](). 

First, we start by defining an algebraic data type which represents each payment 
type by a separate data constructor with precisely the fields required for that 
specific standard. This makes invalid states **unrepresentable** by design. As an
example, you cannot create a `Native` payment with a token field or an `ERC721` 
payment with an amount field.

```
data Payment =
    Native (address,word)
  | ERC20 (address,address,address,word)  
  | ERC721(address, address,address,word)
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
since it is not possible to represent invalid states, no runtime validation 
using `require` is necessary, since they are enforced by Core Solidity type system.

In this short example, we can see how Core Solidity gives the developer the tools 
to write shorter and safer code. By using algebraic data types and pattern matching, 
we can avoid the manipulation of invalid states and the need of runtime validations 
using `require`. Also, parametric polymorphism (a.k.a. generics) and type classes 
opens up new possibilities for the development of safer libraries for the modular 
development of smart-contracts.

## Current status 

Core Solidity prototype is being actively developed by Argot's Programming Languages 
Research team and it currently supports the following features: 

- Definition of algebraic data types, pattern matching, type classes and polymorphic functions. 

- Data Locations represented as types: Data locations (e.g., storage, memory) 
are now just standard library types. This enables the creation of composite 
types with  mixed locations, such as `Broker` type which holds references to storage 
arrays and memory data, as presented in the next code piece:

```
data Broker = Broker (memory(word), storage(array(address, word)))
```

- Classic Solidity compatible ABI encoding / decoding, mappings and contract constructors.

Using these features, we can implement a [ERC20 contract in Core Solidity]() which is similar 
to the Classic Solidity version.

## What's next?

The evolution of Solidity has reached a crucial moment. While the language's 
established features have successfully powered the ecosystem, certain patterns have 
revealed limitations in scalability, security, and clarity. This blog post outlines 
Argot's Collective vision for Core Solidity, a deliberate re-design of the language's 
foundation. This initiative aims to replace legacy mechanisms with more robust, 
composable, and community-driven primitives, ensuring the language remains a secure 
and efficient basis for the next generation of smart contracts. 

Our next steps will involve: 

- Inheritance replacement: While a basic building block of Classic Solidity, inheritance has 
often failed as a clean code reuse mechanism. Type classes are our intended, more robust and 
composable replacement in the new language.

- No more `try`/ `catch`: The `try`/`catch` construct has always been problematic in Solidity. 
Instead of fixing its deficiencies, Core Solidity will rely on pattern matching against error 
objects to provide explicit error handling that address all the corner cases `try`/`catch` 
misses.

- A better high-level `delegatecall` mechanism: While not final, traditional libraries are likely 
to be replaced. Free functions and modules can adequately replace internal library functions. We 
are designing a new, first-class mechanism for splitting contracts and connecting pieces via 
`delegatecall` to replace the role of external libraries.

- A community-driven standard library: An overarching goal of Core Solidity is to have a
simple, flexible language core, with much of the current built-in functionality defined 
in-language as a standard library. Currently, extending the compiler is a complex task that 
doesn't fully leverage the wealth of knowledge within our application developer community. 
We aim to establish a community-driven, EIP-style process for the standard library, 
encouraging extensions to be developed this way. Whether this library remains a minimal set 
of utilities or grows into a full-featured toolkit will be decided by the community through 
this process.

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
