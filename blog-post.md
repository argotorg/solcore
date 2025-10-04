# Introducing Core Solidity

Solidity earned its place as the de facto language for smart contracts by 
prioritizing developer accessibility. Its early design leveraged familiar 
high-level abstractions to onboard programmers.
While safety was always a goal, it was the history of contract exploits that 
forged it into the language's highest priority. This focus on security 
transformed Solidity, making it stricter over time.
This necessary changes, however, came with consequences. The language 
now exhibits the scars of its accumulated complexity and a type system 
that has become a barrier to its own evolution. Foundational features like 
generics (aka parametric polymorphism) remain out of reach, as integrating them 
into the current architecture is both difficult and dangerously prone to 
introduce critical flaws.

Aiming to break this impasse, the Argot collective decided to embark on a 
fundamental rebuild, which is named Core Solidity: a project to define a new, 
foundational version of the language designed for security, clarity, and 
future growth from the ground up.


## What is Core Solidity?

The design of Core Solidity took inspiration from well established concepts from functional 
programming, with Haskell serving as a primary influence, while incorporating rigorous 
safety guarantees inspired by modern systems programming languages such as Rust. 
Although this architectural direction represents a significant evolution, it keeps 
the domain-specific requirements of smart contract development, preserving 
the established conventions and practical utility of Classic Solidity where appropriate.

The language's theoretical foundation comprises several formally-specified features that 
provide enhanced type safety and expressivity. In what follows, we provide an overview 
of such features. However, we warn the reader that the current Core Solidity prototype 
uses a syntax which is similar, but not identical, to Classic Solidity. Since our initial 
concern is to develop the new language type system and its semantics. The surface syntax 
would probably change in the near future. All presented examples work in the current 
prototype.

### Generics and Type Classes: Formal Abstraction Mechanisms

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




Code Example:
solidity

// Type class definition specifying algebraic structure
trait Monoid<T> {
    function empty() external pure returns (T);
    function combine(T a, T b) external pure returns (T);
}

// Generic implementation constrained by type class
impl Monoid<uint256> for uint256 {
    function empty() public pure returns (uint256) {
        return 0;
    }
    
    function combine(uint256 a, uint256 b) public pure returns (uint256) {
        return a + b;
    }
}

- Generics and Type Classes

- Hindley-Milner Type Inference

- First-Class Functions

- Algebraic Data Types

- Pattern Matching and Destructuring

This core is, in fact, a complete language in itself, provisionally called SAIL 
(Solidity Abstract Intermediate Language). Core Solidity is built on top of SAIL by 
providing more specialized syntactic constructs that replicate familiar features from 
Classic Solidity, alongside a mechanism for "desugaring" them back into the pure core 
syntax. What Does Core Solidity brings to the Table?

The new foundation allows us to introduce several elements we have always wanted in 
Solidity:

- New Copy/Reference Semantics: In Classic Solidity, an assignment can sometimes copy 
a value and sometimes just a reference, depending on context --- a behavior that can be 
unintuitive. Core Solidity distinguishes between the two at the syntax level, making the 
behavior explicit and predictable.

- Data Locations Tied to Types: Data location (e.g., storage, memory) is now an integral 
part of a type. This enables the creation of composite types with mixed locations, such 
as a memory struct that holds references to storage arrays.

- Control Flow as Expressions: Most control-flow elements—including loops, conditions, 
and blocks—become expressions rather than statements. They return values and can be 
seamlessly used within more complex expressions.

- Operator Overloading with Heterogeneous Arguments: We consciously limited operator 
overloading in Classic Solidity to homogeneous types. With the new type system nearing 
completion, we can confidently support operators with parameters and return values of 
different types.

- Universal Static/Dynamic Array Literals: Classic Solidity limits array literals to static 
arrays. The new type system, powered by robust type inference, allows literals to adopt the 
correct type (static or dynamic) from their usage context from the start.

- Full Compile-Time Constant Evaluation: While technically possible in Classic Solidity, 
full compile-time evaluation is complex. Core Solidity will support it comprehensively, 
including for arbitrary reference types.

- Redesigned Module System: (TODO: Briefly outline plans for the module system here.)

- Macros: One of the key design principles behind Core Solidity design to make it extensible
in order to avoid 


## What is Changing or Being Removed?

With new power comes simplification. Some features are being rethought or replaced:

- Inheritance replacement: While a basic building block of Classic Solidity, inheritance has 
often failed as a clean code reuse mechanism. Type classes are our intended, more robust and 
composable replacement in the new language.

- No more `try`/ `catch`: The try/catch construct has always been a leaky abstraction in Solidity. 
Instead of patching its deficiencies, Core Solidity will rely on pattern matching against error 
objects and fundamental control flow to provide explicit, granular error handling that addresses 
all the corner cases try/catch misses.

- A Better high-level `delegatecall` mechanism: While not final, traditional libraries are likely 
to be replaced. Free functions and modules can adequately replace internal library functions. We 
are designing a new, first-class mechanism for splitting contracts and connecting pieces via 
`delegatecall` to replace the role of external libraries.

## A Community-Driven Standard Library

An overarching goal of Core Solidity is to have a simple, flexible language core, with much of 
the current built-in functionality defined in-language as a standard library.

Currently, extending the compiler is a complex task that doesn't fully leverage the wealth of 
knowledge within our application developer community. We aim to establish a community-driven, 
EIP-style process for the stewardship of this standard library, encouraging extensions to be 
developed this way. Whether this library remains a minimal set of utilities or grows into a 
full-featured toolkit will be decided by the community through this process.

We also want to prevent language fragmentation. With more compiler implementations emerging, we 
believe users are best served by a unified language specification. Compilers can then compete on 
optimization quality and alternative standard library implementations. Core Solidity is designed 
to provide both a clear specification and a reference foundation.

Standard Library Content will, at a minimum, include:

- ABI encoding/decoding helpers.

- Error handling helpers.

- Support for standards (e.g., typehash from EIP-712).

- Built-ins like bytes.concat(), type(T).max, and type(I).interfaceId.

- Denominations (wei, ether, week, etc.).

- Mathematical utilities.

## Conclusion

(TODO: Add a concluding paragraph that invites readers to comment on the forum, specifically asking them to prioritize the features on the wishlist discussed above.)
Roadmap

### Phase 1 (Stabilization)

- Finalize the approach to compile-time evaluation (comptime).

- Implement the module system.

- Achieve feature compatibility with solc.

### Phase 2 (Production Readiness)

- Foster community collaboration on the standard library.

- Develop a production-ready implementation.

- Formalize the specification and create a reference interpreter.

This version maintains your technical precision while making the narrative more fluid and engaging. It clearly frames Core Solidity as an ambitious and necessary evolution. Let me know if you'd like to refine any specific section further
