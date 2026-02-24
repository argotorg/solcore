# Module and Namespace System

This document describes the module behavior currently implemented in solcore.

## Module Identity and File Mapping

- Module names are file-system driven.
- `import foo;` resolves to `foo.solc`.
- `import foo.bar;` resolves to `foo/bar.solc`.
- The compiler searches import roots in order:
  1. directory of the entry file
  2. directories passed via `--include` (default: `std`)
- Canonicalized file paths are used as module identities.

## Imports

Supported forms:

```solidity
import M;
import M as N;
import M.{X, Y};
import M.{*};
```

- `import M;` and `import M as N;` add only a qualifier.
- `import M.{...};` brings selected exported names into unqualified scope.
- `import M.{*};` brings all exported names into unqualified scope.
- Open-import behavior is not supported.
- Duplicate names inside a selective import are rejected.

## Exports

Supported forms:

```solidity
export {X, Y};
export {*};
```

- Imported modules must declare exactly one `export { ... };`.
- `export {*};` exports all importable top-level declarations.
- Duplicate names in exports are rejected.
- Exporting unknown names is rejected.

## Name Resolution

Unqualified lookup order:

1. Local lexical scope.
2. Current module top-level declarations.
3. Names imported by `import M.{...};` (including glob selectors).
4. Otherwise unresolved.

Ambiguity across selected imports is a compile error.

Namespaces are split:

- Type namespace: data/type constructors, type synonyms, classes, contracts.
- Term namespace: functions, methods, constructors, values/variables.

The same spelling can appear once in each namespace. Duplicates in the same namespace are errors.

## Dot Constructor Shorthand

The parser supports contextual constructor shorthand:

```solidity
.Some(1)
.None
```

Behavior:

- Expression shorthand requires an expected constructor result type from context.
- Pattern shorthand requires an expected scrutinee type from context.
- If context does not provide a constructor-bearing expected type, compilation fails.
- If no constructor matches, or multiple constructors match, compilation fails.

## Pragmas and Cycles

- Import cycles are rejected during module graph construction with a concrete cycle chain.
- Pragmas are module-local and do not propagate through imports.
