# Comptime Integer Type: Design and Implementation

## Overview

`integer` is a compile-time-only type for unlimited-precision integer arithmetic.
It is the intended eventual type of all integer literals in Solcore.  All six
implementation steps have been completed; bare integer literals at `integer`-typed
sites are automatically coerced by the desugaring pass.

### Motivation

Using `word` for compile-time arithmetic has two weaknesses:

- Arithmetic is silently reduced mod 2^256; intermediate overflow can corrupt
  a compile-time computation even if the final result fits in 256 bits.
- There is no way for the type system to distinguish "this value exists only at
  compile time" from "this value exists at runtime".

A compile-time-only `integer` type solves both: arithmetic is exact (backed by
Haskell's `Integer`), and the type system prevents `integer` values from
appearing in generated EVM code.

### Two Forms of Comptime Functions

The design supports two coexisting forms:

**Form 1 — `word -> word`** (works today; no changes needed):
```solidity
import std;

function fib(comptime n : word) -> comptime word {
    if (n < 2) { return n; }
    else { return fib(n-1) + fib(n-2); }
}
let res : word  = fib(10);        // comptime-folded
let res : word  = fib(runtimeX);  // evaluated at runtime
```
Uses word arithmetic; MastEval folds it when the argument is known at
compile time.  Can also run at runtime.  Requires `import std` for `<`.

**Form 2 — `integer -> integer`** (requires this work; standalone, no std):
```solidity
function fib(comptime n : integer) -> comptime integer {
    if (integerLt(n, wordToInteger(2))) { return n; }
    else {
        return integerAdd(fib(integerSub(n, wordToInteger(1))),
                          fib(integerSub(n, wordToInteger(2))));
    }
}
// comptime only:
let res : word = wordFromInteger(fib(wordToInteger(10)));
```
Arithmetic is exact; the comptime-only type enforces that the result must be
converted before any runtime use.  Uses only compiler builtins — no `import std`.

---

## The `integer` Type

### Properties

| Property | Value |
|---|---|
| Type representation | `TyCon "integer" []` — a nullary type constructor |
| Precision | Unlimited (backed by Haskell `Integer`) |
| Runtime existence | None — must be fully eliminated before hull emission |
| Comptime-only | Values may only appear in `comptime` bindings or as return values of `comptime`-annotated functions |
| Literal syntax | `let x : integer = 42` — bare literals coerced by `desugarIntegerLiterals` pass |

### MAST representation

`integer` values are carried as `MastLit (IntLit n)` with MAST type
`MastTyCon "integer" []`.  The value representation is **identical** to `word`
literals — Haskell's `Integer` already holds arbitrary-precision values; only
the type tag differs.

**Design note — unityped literals**: `MastLit (IntLit n)` carries no type tag.
The distinction between `word` and `integer` literals is recorded only in the
surrounding `MastLet`/`MastParam` `MastId` (via `mastIdType`), not in the
literal itself.  Consequently `typeOfMastExp (MastLit (IntLit _))` always
returns `word` and is unreliable for `integer`-typed values.  The PoC is safe
because all `integer`-typed comptime bindings are folded by MastEval and
eliminated before EmitHull sees them, so the ambiguity is never exposed at
emission.  Carrying a type tag in `MastLit` is the correct long-term fix but
is an AST change beyond PoC scope.

---

## Primitive Functions

All functions below are registered in `Primitives.hs` and are available
everywhere without `import std`.  `bool` is a compiler builtin
(`boolTy = TyCon "bool" []` in `Primitives.hs`), so functions returning `bool`
are likewise available without std.

The complete set of primitive names is exported as `Primitives.integerPrimNames`
and is the single source of truth used by `MastEval.builtinPureFuns`,
`Specialise.comptimeBuiltins`, and `IntegerLiteralDesugar.integerBuiltinSigs`.
Add new primitives to `integerPrimNames` and all three consumers update
automatically (except `integerBuiltinSigs` which also needs a type entry).

| Name | Type | Semantics |
|---|---|---|
| `wordToInteger` | `word -> integer` | Lift a 256-bit word into the integer domain. Value-level identity. |
| `wordFromInteger` | `integer -> word` | Truncate: result is `n mod 2^256`. |
| `integerAdd` | `integer -> integer -> integer` | Exact addition. |
| `integerSub` | `integer -> integer -> integer` | Exact subtraction. |
| `integerMul` | `integer -> integer -> integer` | Exact multiplication. |
| `integerLt` | `integer -> integer -> bool` | Exact less-than. Returns compiler builtin `bool`. |
| `integerEq` | `integer -> integer -> bool` | Exact equality. Returns compiler builtin `bool`. |

`wordToInteger` and `wordFromInteger` are one-sided inverses:
`wordFromInteger(wordToInteger(n)) == n` always, but
`wordToInteger(wordFromInteger(k))` may differ from `k` when `k >= 2^256`.

All seven functions are **pure** and are added to `builtinPureFuns` in MastEval.

`integerLt` and `integerEq` serve as the standalone precursors to
`instance integer : Ord` and `instance integer : Eq` in std (future work).

### Standard library: `fromInteger`

`fromInteger :: integer -> a [Num a]` is the overloaded version defined in
`std/NumLib.solc` as a separate step after the core PoC:

- `word:Num` delegates to the builtin `wordFromInteger`
- `uint256:Num` wraps `wordFromInteger` in `Typedef.abs`

This enables `let x : uint256 = Num.fromInteger(fib(wordToInteger(10)))`.

---

## Literals and the desugaring pass

### Implemented approach

`tcLit (IntLit _)` continues to return `word` — this avoids breaking any
existing test that uses integer literals.  The `desugarIntegerLiterals` pass
(Step 6, `Solcore.Desugarer.IntegerLiteralDesugar`) runs before type inference
and inserts `wordToInteger` coercions at literal sites where the expected type
is `integer`:

| Source site | After desugaring |
|---|---|
| `let x : integer = 42` | `let x : integer = wordToInteger(42)` |
| `integerAdd(1, 2)` | `integerAdd(wordToInteger(1), wordToInteger(2))` |
| `fib(10)` where `fib :: integer -> integer` | `fib(wordToInteger(10))` |

Non-literal expressions (variables, calls) are left unchanged; the user writes
explicit `wordToInteger` or `fromInteger` there.

### Remaining path

- `let x = 42` (unannotated) is still rejected — no expected type available
- `let x : uint256 = 42` is not yet handled — requires `Num.fromWord(42)`
- `wordToInteger` remains the explicit escape hatch for runtime `word` values
  entering integer arithmetic (e.g. `wordToInteger(storageSlot)`)
- Future: change `tcLit (IntLit _)` to return `integer` and insert `fromInteger`
  at runtime-type use sites instead of `wordToInteger` at literal sites

---

## MastEval Changes

### `evalPrimitive` additions

```haskell
evalPrimitive (Name "integerAdd") [MastLit (IntLit a), MastLit (IntLit b)] =
    Just (MastLit (IntLit (a + b)))

evalPrimitive (Name "integerSub") [MastLit (IntLit a), MastLit (IntLit b)] =
    Just (MastLit (IntLit (a - b)))

evalPrimitive (Name "integerMul") [MastLit (IntLit a), MastLit (IntLit b)] =
    Just (MastLit (IntLit (a * b)))

evalPrimitive (Name "wordToInteger") [MastLit (IntLit n)] =
    Just (MastLit (IntLit n))               -- value-level identity

evalPrimitive (Name "wordFromInteger") [MastLit (IntLit n)] =
    Just (MastLit (IntLit (maskWord n)))    -- truncate to 256 bits

evalPrimitive (Name "integerLt") [MastLit (IntLit a), MastLit (IntLit b)] =
    Just (mkBool (a < b))                  -- mkBool already exists in MastEval

evalPrimitive (Name "integerEq") [MastLit (IntLit a), MastLit (IntLit b)] =
    Just (mkBool (a == b))
```

### `builtinPureFuns` additions

```haskell
, Name "integerAdd", Name "integerSub", Name "integerMul"
, Name "wordToInteger", Name "wordFromInteger"
, Name "integerLt",  Name "integerEq"
```

### Dead variable elimination

The implemented code eliminates any comptime binding whose init expression is
a `isKnownValue` (literal or constructor of literals), regardless of type:

```haskell
let stmts = case mInit' of
      Just e | ct && isKnownValue e -> []
      _ -> [MastLet ct i ty mInit']
```

This is more aggressive than eliminating only comptime-only types, but safe:
all uses are substituted from VEnv before the let is dropped, and EmitHull's
guard catches any `integer`-typed binding that somehow survives.

### `venvToSubst` invariant

`venvToSubst` converts all `IntLit` values in VEnv into `YulNumber` Yul
literals.  For `integer`-typed values where `n >= 2^256`, this produces an
out-of-range Yul number — but this is harmless because the type checker
prevents `integer`-typed variables from appearing in asm blocks, so the
substitution never finds a match in the Yul AST.  This is a deliberate
reliance on the type-system invariant, not a latent bug; it is documented
here to make the assumption explicit.

---

## ComptimeCheck

### MAST-level checker

`isComptime` already returns `True` for `MastLit _` — no change needed.

`integer`-typed parameters and `let`-bindings must carry the `comptime` flag at
the source level; the existing flag propagation handles them.

**Enforcement gap**: the PoC does not reject `let x : integer = e` without
the `comptime` flag.  If MastEval cannot fold such a binding, it stays in
the AST and EmitHull panics.  Enforcing that `integer`-typed bindings require
`comptime` at the SAIL/MAST checker level is future work.

### SAIL-level checker

No special case needed.  `integer` is a new type constructor; the checker treats
any `comptime`-annotated binding as `CTComptime` regardless of type.

---

## EmitHull Guard

All `integer`-typed values should be folded by MastEval and eliminated by
dead-let removal before EmitHull runs.  A defensive guard catches cases where
this invariant is violated.

Since `typeOfMastExp (MastLit (IntLit _)) = word` regardless of whether the
literal is actually `integer`-typed, the guard cannot use `typeOfMastExp`.
Instead, check the `MastId.mastIdType` from the enclosing constructs:

- **`MastLet`**: if `mastIdType i == mastInteger` → panic
- **Function definition**: if `mastFunReturn fd == mastInteger` → panic
  (catches un-folded `integer`-returning functions that were not inlined)
- **`MastParam`**: if `mastParamType p == mastInteger` in any emitted function
  → panic

These checks together ensure that `integer` as a type does not survive into
hull emission through any path.

---

## Specialization

`integer` is a nullary type constructor with no type variables.  When
specializing a function like `fib :: comptime integer -> comptime integer`,
the specializer produces a monomorphic instance `fib$integer` via the normal
path — no changes to `Specialise.hs` are required.

---

## Standard Library Integration (Step 5)

`fromInteger :: integer -> a [Num a]` in `std/NumLib.solc`:

```solidity
class a : Num {
    function toWord(x : a) -> word;
    function fromWord(x : word) -> a;
    function fromInteger(x : integer) -> a;   // new
    // add, sub, gt, ... (existing)
}

instance word : Num {
    function fromInteger(x : integer) -> word { wordFromInteger(x) }
    // ... existing methods unchanged
}

instance uint256 : Num {
    function fromInteger(x : integer) -> uint256 {
        uint256(wordFromInteger(x))
    }
    // ... existing methods unchanged
}
```

---

## Ordered Implementation Steps

Each step leaves all existing tests passing.  No changes to `tcLit`, `tcStmt`,
or the type checker are required for the PoC.

### Step 1 — `integer` type and primitives (`Primitives.hs`)

- Add `integer :: Ty` alongside `word` and `string`
- Register all seven primitives in the primitives table:
  `wordToInteger`, `wordFromInteger`, `integerAdd`, `integerSub`, `integerMul`,
  `integerLt`, `integerEq`

No other files change at this step.

Note: specialization needed a bypass for the integer primitives.
`Specialise.comptimeBuiltins` (derived from `integerPrimNames`) lists them so
`specCall` passes them through without looking for a `TcFunDef` body.

### Step 2 — MastEval evaluation (`MastEval.hs`)

- Add seven `evalPrimitive` clauses (see above)
- Add seven names to `builtinPureFuns`
- Extend dead-let elimination to use `isComptimeOnly (mastIdType i)` (see above)

Unit tests in `MastEvalTests.hs`:
- `integerAdd` does not overflow: `2^255 + 2^255 = 2^256` (not 0)
- `wordFromInteger` applies `maskWord`: `wordFromInteger(2^256) = 0`
- `wordToInteger` is value-level identity
- `integerLt (2^256) (2^256 + 1) = true`
- `integerEq (2^256) (2^256) = true`

### Step 3 — EmitHull guard (`EmitHull.hs`)

Add panic clauses using `mastIdType`/`mastFunReturn`/`mastParamType` (not
`typeOfMastExp`) as described in the EmitHull Guard section above.

Verified by running full test suite (the guard must not fire).

### Step 4 — Integration tests

**`test/examples/comptime/integer-fib.solc`** — no `import std` needed:

```solidity
function fib(comptime n : integer) -> comptime integer {
    if (integerLt(n, wordToInteger(2))) { return n; }
    else {
        return integerAdd(
            fib(integerSub(n, wordToInteger(1))),
            fib(integerSub(n, wordToInteger(2)))
        );
    }
}

contract FibInteger {
    function main() -> word {
        let res : comptime word = wordFromInteger(fib(wordToInteger(10)));
        return res;
    }
}
```

Expected: `main` folds to a constant returning `55`.

**`test/examples/comptime/integer-basic.solc`** — exercises primitives directly:

```solidity
contract IntegerBasic {
    function main() -> word {
        let x : comptime integer = wordToInteger(42);
        let y : comptime integer = integerAdd(x, wordToInteger(8));
        return wordFromInteger(integerMul(y, wordToInteger(2)));
    }
}
```

Note: intermediate `integer` lets must carry `comptime` so MastEval folds and
eliminates them (EmitHull cannot emit `integer`-typed variables).

Expected: folds to constant `100`.

### Step 5 — `Num.fromInteger` in stdlib ✓

- Added `fromInteger(comptime x: integer) -> comptime a` to the `Num` class
  and default instance in `std/std.solc`; the default implementation uses
  `Typedef.abs(wordFromInteger(x))` which works for `word` (identity) and
  `uint256` (wrapping constructor)
- Test: `test/examples/comptime/integer-from-integer.solc` exercises both
  instances: `Num.fromInteger(wordToInteger(42)) : word = 42` and
  `Num.fromInteger(fib(wordToInteger(10))) : uint256 = uint256(55)`

### Step 6 — Literal desugaring pass ✓

`Solcore.Desugarer.IntegerLiteralDesugar.desugarIntegerLiterals` runs before
type inference on the untyped AST.  `tcLit` is **not** changed; literals remain
`word`-typed.  The pass inserts `wordToInteger` coercions at integer literal
sites where the expected type is `integer`, determined from:

- Explicit let type annotations: `let x : integer = 42`
- Function return types: `return 42` in `-> integer` functions
- Declared parameter types: `integerAdd(1, 2)` via the signature table

The signature table is built from hardcoded integer builtin signatures
(names + param types) plus all user-defined and class/instance signatures
collected from the CompUnit.  Only `TyCon "integer" []` triggers coercion;
polymorphic params, unknown functions, and unannotated lets are left unchanged.

Tests: `integer-lit.solc` (positive) and `integer-lit-safe.solc` (no spurious
coercions in word arithmetic, already-wrapped args, word-annotated lets).

---

## Known Gaps and Future Work

| Gap | Impact | Resolution |
|---|---|---|
| ~~`let x : integer = 42` (bare literal)~~ | ~~Requires `wordToInteger(42)`~~ | ✓ Step 6 |
| ~~`integerAdd(42, 3)` with bare literals~~ | ~~Requires `wordToInteger` wrappers~~ | ✓ Step 6 |
| `instance integer : Add/Sub/Mul` | Cannot write `n + m` for `integer` | Std instances (future) |
| `instance integer : Eq/Ord` | Use `integerLt`/`integerEq` in the interim | Std instances (future) |
| `let x = 3` (unannotated) | Rejected — desugaring pass needs explicit annotation | Future: default to `word` |
| `let x : uint256 = 3` | Requires `Num.fromWord(3)` or `uint256(3)` | Full Zig-style desugaring (future) |
| `integer` binding without `comptime` | EmitHull panic if not folded | Enforce `comptime` in SAIL/MAST checkers |
| `typeOfMastExp` for `integer` literals | Returns `word` — unreliable | Carry type tag in `MastLit` (AST change) |
| `integerBuiltinSigs` param types | Must be manually synced with `Primitives.hs` | Could derive from `Scheme` objects (future) |
