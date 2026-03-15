# Module and Namespace System

This document describes the current Solcore implementation.

## 1. Entry File and Library Roots

- The entry file is the path passed to `-f` / `--file`.
- The main library root is:
  1. `takeDirectory(entryFile)` by default, or
  2. the first path from `--include` when `--include` is provided.
- The std library root is the second include root. With the default options this is `std`.
- External libraries are registered explicitly with repeated `--lib NAME=DIR` flags.
- Additional `--include` entries beyond the first two are currently ignored by the module loader.

## 2. Module Identity and File Mapping

- Module identity is logical, not canonical-path-based.
- A module id is `(library, logical module path)`.
- Libraries are:
  - the main library
  - the std library
  - named external libraries
- `foo.bar` maps to `foo/bar.solc`.
- Directories are not modules.
- `foo` and `foo.bar` therefore refer to different files.
- The source file path is stored separately from module identity.
- If two logical module paths reach the same physical file, they are still treated as different modules when their module ids differ.

## 3. Import Syntax

Supported forms:

```solidity
import M;
import M as A;
import M.{X, Y};
import M.{*};
import lib.foo.bar;
import @ext.foo.bar;
```

Import path kinds:

- `import foo.bar;`
  - relative to the importing module's logical directory
- `import lib.foo.bar;`
  - from the current library root
- `import @ext.foo.bar;`
  - from the external library registered as `ext`

Current std-specific behavior:

- `import std;` resolves to the std library root from any library.
- For modules in the main library, unresolved bare imports also fall back to the std root.
  This keeps forms such as `import dispatch;` working.

## 4. Import Visibility and Qualification

- `import M;` does not open names into unqualified scope.
- `import M as A;` binds only `A`.
- `import M.{X, Y};` imports selected exported names into unqualified scope.
- `import M.{*};` imports all exported item names into unqualified scope.
- Items inside `{...}` must be simple item names or `*`.
  Dotted item paths are not supported there.

Default module bindings:

- `import foo.bar;` binds `bar`.
- `import foo.bar as B;` binds `B` and does not bind `bar`.
- Non-alias module imports also support full-path qualification, so `import foo.bar;` allows both `bar.x` and `foo.bar.x`.
- If two imports would bind the same final segment, it is an error.
  For example, `import foo.bar; import baz.bar;` is rejected.

Validation rules:

- Duplicate qualifier names are rejected.
- Duplicate names inside one selective import are rejected.
- Unknown selected names are rejected.
- Ambiguous names introduced by selective or glob imports are rejected.

## 5. Export Syntax and Public Interfaces

Supported forms:

```solidity
export {X, Y};
export {*};
export {M.*, *};
export M;
export M as N;
export M.{X, Y};
export M.*;
```

Current behavior:

- A module may contain multiple export declarations.
- The public interface is the union of all export declarations after expansion.
- If a module has no export declarations, its public interface is empty.
- `export` declarations do not add names to the current module's local scope.
- Only exported names and re-exported modules are visible to importers.

Expansion rules:

- `export {X}` exports a local item.
- `export {*} ` exports all local importable top-level items.
- `export {M.*}` re-exports all exported item names from `M`.
- `export M;` re-exports the module under its final segment.
- `export M as N;` re-exports the module under `N`.
- `export M.{X, Y};` re-exports selected item names from `M`.
- `export M.*;` re-exports all exported item names from `M`, but not the module binding itself.

Validation rules:

- Unknown local exports are rejected.
- Unknown re-exported names are rejected.
- Duplicate exported item names are rejected after expansion.
- Duplicate exported module binding names are rejected after expansion.

Instance behavior:

- Instances are import-visible whenever their defining module is imported.
- Instances are not named individually in export lists.

## 6. Namespaces and Name Resolution

Current duplicate checking is enforced separately for:

- the type namespace
  - contracts
  - data types
  - type synonyms
- the class namespace
- the term namespace
  - functions
  - constructors
  - values

Unqualified lookup order:

1. Local lexical scope
2. Current module top-level declarations
3. Names introduced by `import M.{...}` / `import M.{*}`
4. Otherwise unresolved

## 7. Constructors and Dot Shorthand

Constructors remain term-level names, but they are canonicalized after resolution to `Type.Constructor` form.

Examples of accepted source forms:

- `Option.Some`
- `mod.Option.Some`
- `alias.Option.Some`
- `.Some` when expected type context is available

Current behavior:

- Bare constructor names are not resolved by default in the qualified-constructor model.
- A constructor can still be used unqualified when it is explicitly brought in through selective import.
- `data Foo = Foo` remains valid because type and term namespaces are separate.

Dot shorthand:

- `.K` is resolved during contextual typing, not during the initial name-resolution pass.
- Pattern shorthand uses the expected scrutinee type.
- Expression shorthand uses the surrounding expected type.
- Nested shorthand works when outer context determines the constructor family.
- Missing expected type, non-data expected type, or missing constructor name is an error.

## 8. Pragmas

- Pragmas are module-local.
- Pragmas do not propagate through imports.
- Imported instances are trusted during checking without importing the defining module's pragma environment.

## 9. Recursive Modules

- Import cycles are allowed.
- The loader computes strongly connected components of the import graph.
- A recursive component is compiled as a recursive module group.
- Public interfaces inside a recursive group are computed by fixed-point iteration.
- Compile surfaces for recursive groups are also computed by fixed-point iteration.
- Self-imports and mutual re-export cycles are therefore supported.
- If interface or compile-surface expansion does not stabilize, compilation fails for the group.

## 10. External Libraries

- External libraries are configured with `--lib NAME=DIR`.
- Source code imports them with `@NAME.module.path`.
- The external library name is part of module identity.
- Relative imports inside an external library stay within that external library.
- `lib.*` inside an external library resolves from that external library's root.
- Version selection and dependency lockfiles are not part of the language-level implementation yet.
