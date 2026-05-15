# SAIL

SAIL (Solidity Advanced Intermediate Language) is the source language of the
Core Solidity compiler. It extends Solidity's surface syntax with a
statically-typed functional core: parametric polymorphism via `forall`
quantifiers, type classes for constrained overloading, and algebraic data types
with exhaustive pattern matching. Every SAIL program is compiled to monomorphic
Core IR through specialization, then translated to Yul and assembled into EVM
bytecode.

This section documents the SAIL language itself. The chapters are ordered from
the most foundational concepts to the most advanced:

- **Syntax** covers lexical conventions, literals, and the overall structure of
  a source file.
- **Built-ins** lists the types, values, and operators available without any
  import, along with the full set of EVM opcodes accessible inside `assembly`
  blocks.
- **Variable Declaration and Assignment** explains `let` bindings and mutation.
- **Functions** describes function signatures, free functions, contract methods,
  and recursive definitions.
- **Assembly Blocks** details the rules for embedding raw Yul inside a SAIL
  function body.
- **Datatypes** introduces algebraic data type declarations and pattern
  matching.
- **Parametric Polymorphism** explains `forall` quantifiers, type variable
  instantiation, and the specialization strategy.
- **Type Classes** covers class declarations, instance declarations, superclass
  constraints, and the three soundness conditions the compiler enforces.
- **Modules** describes the import and export system, qualified names, and
  visibility rules.
- **Type Inference** explains the bidirectional constraint-based algorithm,
  what must be annotated explicitly, and the error messages the compiler
  produces when inference fails.
