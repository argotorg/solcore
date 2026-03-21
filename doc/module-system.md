# Module and Namespace System

This document describes the intended Solcore module and namespace system.

## 1. Entry File and Library Roots

- The entry file is the path passed to `-f` / `--file`.
- The main library root is configured with `--root`.
- The default main library root is the current working directory.
- The std library root is configured with `--include`. With the default options this is `std`.
- External libraries are registered explicitly with repeated `--lib NAME=DIR` flags.

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
import M.{*} hiding {X};
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
- `import std.dispatch;` resolves to `dispatch.solc` under the std root.
- Bare imports do not fall back to the std root.
- Imports do not have constructor-specific selector syntax.
  Exported constructors are accessed through qualified constructor paths such as `T.C`, `M.T.C`, or `alias.T.C`.

## 4. Import Visibility and Qualification

- `import M;` does not open names into unqualified scope.
- `import M as A;` binds only `A`.
- `import M.{X, Y};` imports selected exported names into unqualified scope.
- `import M.{*};` imports all exported item names into unqualified scope.
- `import M.{...} hiding {X, Y};` removes names from the selector result after expansion.
- Items inside `{...}` may mix simple item names and `*`.
  Dotted item paths are not supported there.

Default module bindings:

- `import foo.bar;` binds `bar`.
- `import foo.bar as B;` binds `B` and does not bind `bar`.
- Non-alias module imports also support full-path qualification, so `import foo.bar;` allows both `bar.x` and `foo.bar.x`.
- If two imports would bind the same final segment, it is an error.
  For example, `import foo.bar; import baz.bar;` is rejected.

Validation rules:

- Duplicate qualifier names are rejected.
- Duplicate explicit names inside one selective import are rejected.
- Duplicate names inside one `hiding {...}` list are rejected.
- Unknown selected names are rejected.
- Unknown hidden names are rejected.
- Ambiguous names introduced by selective or glob imports are rejected.

## 5. Export Syntax and Public Interfaces

Supported forms:

```solidity
export {X, Y};
export {*};
export {T(C1, C2)};
export {T(*)};
export {M.*, *};
export M;
export M as N;
export M.{X, Y};
export M.{T(C1, C2)};
export M.{T(*)};
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
- `export {T(C1, C2)}` exports the local data type `T` and the named constructors of `T`.
- `export {T(*)}` exports the local data type `T` and all constructors of `T`.
- `export {*} ` exports all local importable top-level items, but not subordinate constructors.
- `export {M.*}` re-exports all exported item names from `M`.
- `export M;` re-exports the module under its final segment.
- `export M as N;` re-exports the module under `N`.
- `export M.{X, Y};` re-exports selected item names from `M`.
- `export M.{T(C1, C2)};` re-exports the imported data type `T` and the named constructors of `T` that are visible through `M`.
- `export M.{T(*)};` re-exports the imported data type `T` and all constructors of `T` that are visible through `M`.
- `export M.*;` re-exports all exported item names from `M`, preserving constructor visibility on exported data types, but not the module binding itself.

Constructor-export rules:

- Constructor names are not exported as standalone top-level items.
  They must be exported through a data type item such as `T(C1, C2)` or `T(*)`.
- `T(C1, C2)` and `T(*)` always imply export of the type `T` itself.
- `*` never implies export of constructors.

Validation rules:

- Unknown local exports are rejected.
- Unknown re-exported names are rejected.
- Naming a constructor that does not belong to the selected data type is rejected.
- Re-exporting a constructor that is not visible through the source module is rejected.
- Re-exporting two different underlying items under the same public name is rejected.
- Re-exporting two different module targets under the same public module name is rejected.
- Repeated exports of the same underlying item are normalized, so forms such as `export {main, *};` are accepted.

Instance behavior:

- Instances are import-visible whenever their defining module is imported.
- Instances are not named individually in export lists.

## 6. Namespaces and Name Resolution

Current duplicate checking is enforced separately for:

- the type namespace
  - contracts
  - data types
  - type synonyms
  - classes
- the term namespace
  - functions
  - constructors
  - values

Unqualified lookup order:

1. Local lexical scope
2. Current module top-level declarations and names introduced by `import M.{...}` / `import M.{*}` are treated at the same priority
3. If that combined non-local set contains more than one candidate in the same namespace, validation fails with a hard error
4. Otherwise unresolved

Current behavior:

- Local parameters and local variables still shadow non-local names.
- Current-module top-level names no longer silently shadow selected or glob-imported names.
- Selected or glob-imported names no longer silently shadow current-module top-level names.
- Re-importing the same underlying declaration is normalized, so importing `std.{*}` and then `std.{uint256}` does not fail by itself.

## 7. Constructors and Dot Shorthand

Constructors remain term-level names, but they are canonicalized after resolution to `Type.Constructor` form.

Constructor visibility:

- `export {T}` exports the type only; external modules cannot name any constructors of `T`.
- `export {T(C1, C2)}` exports only the named constructors of `T`.
- `export {T(*)}` exports all constructors of `T`.
- Imports do not add constructors as unqualified names.
  Instead, exported constructors are accessed through qualified paths such as `T.C`, `mod.T.C`, and `alias.T.C`.

Examples of accepted source forms:

- `Option.Some`
- `mod.Option.Some`
- `alias.Option.Some`
- `.Some` when expected type context is available

Current behavior:

- Bare constructor names are not resolved by default in the qualified-constructor model.
- `data Foo = Foo` remains valid because type and term namespaces are separate.
- A hidden constructor cannot be named from another module in either expressions or patterns.

Dot shorthand:

- `.K` is resolved during contextual typing, not during the initial name-resolution pass.
- Pattern shorthand uses the expected scrutinee type.
- Expression shorthand uses the surrounding expected type.
- Nested shorthand works when outer context determines the constructor family.
- `.K` only resolves if `K` is visible from the current module on the expected data type.
- Missing expected type, non-data expected type, or missing constructor name is an error.

Pattern matching and exhaustiveness:

- Inside the defining module, pattern matching sees the full constructor set of a data type.
- Outside the defining module, only visible constructors may appear in patterns.
- If some constructors of a data type are hidden from the current module, matches on that type are treated as non-exhaustive unless they contain a wildcard or catch-all arm.
- If all constructors of a data type are visible, ordinary exhaustiveness checking applies.

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
