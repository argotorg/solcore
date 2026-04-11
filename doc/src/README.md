# Introduction

## Purpose and Audience

Core Solidity is an update to the [Solidity](https://docs.soliditylang.org/en/latest/) programming
language. It both extends the language with new features (ADTs, Parametric polymorphism,
Typeclasses), and diverges from Classic Solidity in incompatible ways (most notably by removing
inheritance). This document targets compiler implementers, tooling developers, and language
researchers. It is a reference specification, not a tutorial.

A prototype implementation exists at [`argotorg/solcore`](https://github.com/argotorg/solcore). The
implementation is a research prototype. It contains bugs, lacks optimizations, and will change.

## Compilation Architecture

Compilation proceeds by succesive translation between the following representations:

1. **Core Solidity**: The user-facing language with all syntactic features. Supports higher-order
   functions, pattern matching, type classes, and modules. This is a superset of SAIL, with all
   additional language constructs being defined in terms of desugaring passes (syntactic
   transformations) into SAIL.

1. **SAIL**: A minimal first-order intermediate representation. All syntactic sugar has been
   removed. Type classes are resolved to concrete instances. Polymorphic functions are specialized
   to monomorphic versions. Pattern matching is compiled to simple case trees.

1. **Hull**: A low-level representation close to Yul. Retains algebraic data types (binary sums and
   products) but otherwise resembles Yul. Hull code is translated directly to Yul, which is then
   compiled to EVM bytecode.

1. **Yul**: The existing [Yul](https://docs.soliditylang.org/en/v0.8.35-pre.1/yul.html) assembly
   language currently used in solc. Core Solidity is designed in a way that allows the use of
   alternative or additional representations at this level in the future.

Each transformation step simplifies the language and moves closer to the EVM execution model.

## Document Structure

- **Compilation Pipeline**: Describes the compilation pipeline and how representations relate
- **SAIL**: Specifies the desugared intermediate representation
- **Core Solidity**: Specifies the user-facing language features
- **Hull**: Specifies the low-level IR before Yul emission

## Status

This is a research prototype. The language design is not stable. Features may be added, removed, or
changed. The implementation has known bugs and missing functionality. Do not use this for production
systems.
