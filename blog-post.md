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
future. All presented examples work in the current prototype.

### Generics and Type Classes

Core Solidity will introduce two new exciting abstraction mechanisms: generics and 
type classes.

Generics enable parametric polymorphism through type parameters, which allows the  
implementation of algorithms and data structures that operate uniformly across 
over all types. As an example, we could define a polymorphic `identity` function:

``` 
forall a . function identity(x : a) -> a {
   return x;
}
```

While generic functions are interesting, most interesting operations are not defined 
at all types. Overloading allows the definition of code which can operate in distinct 
ways at different types. Type classes are the standard way of combining overloading and 
parametric polymorphism (generics) in a systematic manner. A type class definition 
declares the class name, its arguments and member functions type signature. As an 
example, let's consider the task of defining addition over different types:
```
function a . class a : Sum {
  function sum (x : a, y : a) -> a;
}
```
Implementations for member functions for different types are provided by instance 
definitions. As an example, let's consider the implementation of `Sum` for 
`word` type. 
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
Type Classes allows the constrain these type parameters by specifying required 
interfaces, establishing compile-time guarantees about type capabilities. This 
approach provides a more mathematically sound alternative to inheritance-based 
polymorphism.

### High-order functions

Functions possess first-class status within the type system, enabling their use
as parameters, return values, and assignable entities. This facilitates the
implementation of higher-order functions and functional composition patterns,
enhancing expressive capability while maintaining referential transparency.

```
forall T U . function map (memory(array(T)) input, transform : (T) -> U) : memory(array(U)) {
    let result memory(array(U)) = new(memory(array(U)),input.length);
    for (uint i = 0; i < input.length; i++) {
        result[i] = transform(input[i]);
    }
    return result;
}
```

### Type inference

Core Solidity uses type inference algorithm to reduce syntactic verbosity while 
maintaining the strong static typing guarantees essential for secure smart contract 
development. The type inference occurs during compilation and provides complete 
type safety without explicit annotations.

```
let constant_value = 42; // Inferred as uint256
let heterogeneous_tuple = (address(0), true, 42); // Inferred as (address, bool, word)
constant_value = heterogeneous_tuple ; // this is a type error!
```

### Algebraic data types and pattern matching 

Algebraic Data Types provide a way of modeling data modeling using sum types 
(disjoint unions) and product types (structural records). This enables the construction of 
precise types where invalid states are unrepresentable by design, thereby enhancing program 
correctness through the type system itself. As an example, consider the following type 
definition for an auction state. 

```
data AuctionState =
    NotStarted { 
        reservePrice: word  
    } |
    Active { 
        highestBid: word,
        leadingBidder: address 
    } |
    Ended { 
        winningBid: word,
        winner: address 
    } |
    Cancelled;
```
The type has three constructors: `NotStarted` specifies that the action have not started yet and 
stores its reserved price, `Active` denotes that the auction has began and it stores the 
current highest bid and the address that made such bid, `Ended` represents that the auction has 
finished with success and it holds the highest bid and the winner address and constructor 
`Cancelled` is used when the auction has been cancelled.

Using algebraic data types, we can define function by pattern matching. As an example, 
consider function `processAuction` which 
Pattern matching provides structural decomposition of data types through case analysis. This 
ensures all possible variants are handled explicitly, eliminating partial function hazards and 
providing formal guarantees of match completeness.

```
function processAuction(state) {
    match state {
    |   NotStarted(reserve) => {
            require(msg.value >= reserve, "Bid below reserve");
            return Active(msg.value, msg.sender);
        }
    |   Active(currentBid, bidder) => {
            require(msg.value > currentBid, "Bid too low");
            transferFunds(bidder, currentBid); // refund 
            return Active(msg.value, msg.sender);
        }
    |   Cancelled => {
            revert("Auction cancelled");
        }
]   |   _ => {
            return state;
        }
    }
}
```

```
```

### Core Solidity and SAIL

The combination of all these features do form a complete programming language, 
which we call SAIL (Solidity Abstract Intermediate Language). Core Solidity is 
built on top of SAIL by providing more specialized syntactic constructs that 
replicate familiar features from Classic Solidity, alongside a mechanism for 
"desugaring" them into SAIL equivalent code. But what Core Solidity features 
will be translated into SAIL?

- Data Locations represented as types: Data locations (e.g., storage, memory) 
are now a part of a type. This enables the creation of composite types with 
mixed locations, such as a struct that holds references to storage 
arrays and memory data, as presented in the next code piece:

```
struct Broker {
  total : memory(word) 
, accounts : storage(array(address, word))
}
```

- Static/dynamic array - literals: Classic Solidity limits array literals to static 
arrays. Core Solidity type system allows literals to adopt the correct type (static 
or dynamic) from their usage context from the start.

The current Core Solidity prototype support all previous features. In the near 
future, we plan to develop: 

- Compile time evaluation: Inspired by Zig, we plan to include compile-time code 
in Core Solidity. FINISH HERE! 

- Redesigned Module System: WRITE MORE HERE 

- Macros: NEED TO FINISH.


## What's next?

The evolution of Solidity has reached a pivotal moment. While the language's 
established features have successfully powered the ecosystem, certain patterns have 
revealed limitations in scalability, security, and clarity. The following outlines 
Argot's Collective vision for Core Solidity, a deliberate re-design of the language's 
foundation. This initiative aims to replace legacy mechanisms with more robust, 
composable, and community-driven primitives, ensuring the language remains a secure 
and efficient foundation for the next generation of smart contracts. 
The next steps of Core Solidity design will involve: 


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
doesn't fully leverage the wealth of  knowledge within our application developer community. 
We aim to establish a community-driven, EIP-style process for the standard library, 
encouraging extensions to be developed this way. Whether this library remains a minimal set 
of utilities or grows into a full-featured toolkit will be decided by the community through 
this process.

- A formal specification of the language: Having a unified language specification will 
avoid language fragmentation by diverging implementations. Using a unified specification, 
compilers can then compete on optimization quality and alternative standard library 
implementations.

## Conclusion

Core Solidity represents a foundational reimagining of the language, 
designed to equip developers with a more secure, expressive, and mathematically 
sound toolkit for the next generation of smart contracts. The path forward, 
however, will be designed by the community that uses it. We invite you to join 
the discussions and share your perspective. Your input is crucial in helping us 
prioritize this exciting roadmap.
