# Introducing Core Solidity

Solidity earned its place as the de facto language for smart contracts by 
prioritizing developer accessibility. Its early design leveraged familiar 
high-level abstractions to onboard programmers.

While safety was always a goal, it was the history of contract exploits that 
forged it into the language's highest priority. This relentless focus on 
security transformed Solidity, making it stricter over time.
This necessary transformation, however, came with consequences. The language 
now exhibits the scars of its journey-accumulated complexity and a type system 
that has become a barrier to its own evolution. Foundational features like 
generics (aka parametric polymorphism) remain out of reach, as integrating them 
into the current architecture is both difficult and dangerously prone to 
introduce critical flaws.

Tinkering at the edges is no longer a viable path. To break this impasse, 
the Argot collective decided to embark on a fundamental rebuild. This is Core 
Solidity: a project to define a new, foundational version of the language designed 
for security, clarity, and future growth from the ground up.


## What is Core Solidity?

Core Solidity design took inspiration from well established concepts from functional 
programming, with Haskell serving as a primary influence, while incorporating rigorous 
safety guarantees inspired by modern systems programming languages such as Rust. 
Although this architectural direction represents a significant evolution, it keeps 
the domain-specific requirements of smart contract development, preserving 
the established conventions and practical utility of Classic Solidity where appropriate.

The language's theoretical foundation comprises several formally-specified features that 
provide enhanced type safety and expressivity.



Core Solidity draws inspiration from functional languages, primarily Haskell, and
is strongly influenced by modern, safety-oriented languages like Rust. Of course, 
the undeniable influence of "Classic" Solidity is also present.

The core of the language is built around powerful, expressive features:

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
