# Module and Namespace System

This document describes what the current Solcore implementation does today.

## 1. Entry File and Import Roots

- The entry file is the path passed to `-f` / `--file`.
- Import search roots are built in this order:
  1. `takeDirectory(entryFile)`
  2. directories from `--include` (colon-separated, default: `std`)
- Imports are resolved by searching roots in order and picking the first existing file.

## 2. Module Identity and File Mapping

- Module names are file-system driven.
- `import foo;` resolves to `foo.solc`.
- `import foo.bar;` resolves to `foo/bar.solc`.
- Loaded module identity is the canonicalized file path.
- A canonical file is loaded once, even if reached through multiple imports.

## 3. Import Syntax and Semantics

Supported forms:

```solidity
import M;
import M as A;
import M.{X, Y};
import M.{*};
```

Behavior:

- `import M;` and `import M as A;` add qualifier-based access only.
- `import M.{X, Y};` imports selected exported names into unqualified scope.
- `import M.{*};` imports all exported names into unqualified scope.
- There is no open-import semantics.

Validation rules:

- Duplicate qualifier names are rejected (`import A as M; import B as M;`).
- Duplicate names inside one selective import are rejected.
- Unknown selected names are rejected.
- Ambiguous selected imports across modules are rejected.
- Import cycles are rejected, with the cycle chain in the error.

Data constructor selection detail:

- Selective import of a constructor (for example `{True}`) brings its parent data type declaration too, but only with selected constructors.

## 4. Export Syntax and Visibility

Supported forms:

```solidity
export {X, Y};
export {*};
```

Current enforcement:

- Imported modules must declare exactly one export declaration.
- Entry module does not need an export declaration.
- Multiple export declarations are rejected.
- Duplicate names in an export list are rejected.
- Unknown names in an export list are rejected.
- `export {*};` exports all importable top-level declarations (except pragma/export declarations).

Instance behavior:

- Instances are import-visible whenever a module is imported (permissive behavior).
- Instances are not individually name-addressable in export lists.

## 5. Namespaces and Name Resolution

Duplicate checking is enforced separately for:

- type namespace (contracts, data types, type synonyms)
- class namespace
- term namespace (functions, constructors, values)

Unqualified lookup order:

1. Local lexical scope
2. Current module top-level declarations
3. Names introduced by `import M.{...}` / `import M.{*}`
4. Otherwise unresolved

Module qualification:

- Imported qualifiers are added to the resolver environment.
- For nested module paths (for example `import foo.bar;`), prefix qualifiers are also registered for qualified access paths.

## 6. Constructor Model

Current constructor model is type-qualified constructors, with dot shorthand support.

- After name resolution, constructor names are canonicalized to `Type.Constructor` form (for example `Bool.True`, `Option.Some`).
- In source, write constructors as `Type.Constructor`; module/alias prefixes are also supported (for example `mod.Option.Some`, `alias.Option.Some`).
- Bare constructor names (for example `Some`) are not resolved by default in this model.
  Use `Type.Constructor` / `mod.Type.Constructor`, or dot shorthand (`.Some`) when expected type context is available.
- If a constructor is explicitly imported unqualified via selective import, it can be used unqualified.
- `data Foo = Foo` same-name constructors are still accepted.

Dot shorthand:

```solidity
.Some(1)
.None
```

- Expression shorthand requires expected constructor type context.
- Pattern shorthand requires expected scrutinee type context.
- Missing expected type, no match, or ambiguous match is an error.

## 7. Pragma Scope

- Pragmas are module-local.
- Pragmas do not propagate to importing modules.
- Pragmas are not transitive through imports.
