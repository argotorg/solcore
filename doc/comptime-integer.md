# Comptime Integer Type: Design and Implementation Plan

## Overview

`integer` is a compile-time-only type for unlimited-precision integer arithmetic.
It is the intended eventual type of all integer literals in Solcore, though in the
first implementation literals remain typed as `word` and explicit coercions are
required at literal sites.

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
| Literal syntax (PoC) | No new surface syntax; explicit coercion required: `let x : integer = wordToInteger(42)` |
| Literal syntax (target) | `let x : integer = 42` — after the literal desugaring pass lands |

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

## Literals in the PoC: `word` with explicit coercions

### Target design

The target design (described in `comptime-for-core.md`) follows Zig's
`comptime_int` model: **integer literals have type `integer`**, and a
desugaring pass inserts coercions at runtime use sites automatically.
`let a : uint256 = 1` simply works — the compiler inserts `fromInteger(1)`.

### PoC interim

The desugaring pass does not exist yet.  Until it does, switching
`tcLit (IntLit _)` to return `integer` would break every existing test.
The PoC therefore keeps `tcLit` returning `word` and requires explicit
coercions at literal sites:

```solidity
let x : integer = wordToInteger(42);   // lift word literal to integer
let y : word    = wordFromInteger(x);  // lower integer back to word
```

This proves the `integer` type and primitives work end-to-end without
touching any existing test.

### Path to the target

Once the desugaring pass exists:
1. `tcLit (IntLit _)` is changed to return `integer`
2. The pass inserts `fromInteger`/`wordFromInteger` at runtime use sites
3. Explicit `wordToInteger(42)` at literal sites disappears from user code
4. `wordToInteger` remains useful only for lifting a *runtime* `word` variable
   into integer arithmetic (e.g. `wordToInteger(storageSlot)`)

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

`integer`-typed comptime lets must be eliminated alongside comptime string lets.
The current code (from the string-variable work) only eliminates `StrLit`
bindings:

```haskell
-- OLD (eliminates only strings):
let stmts = case mInit' of
      Just (MastLit (StrLit _)) | ct -> []
      _ -> [MastLet ct i ty mInit']
```

Extend using `mastIdType i` (which carries the correct type, unlike
`typeOfMastExp` which returns `word` for all `IntLit`):

```haskell
-- NEW (eliminates all comptime-only types):
let isComptimeOnly ty = ty `elem` [mastString, mastInteger]
    stmts = case mInit' of
          Just e | ct && isKnownValue e && isComptimeOnly (mastIdType i) -> []
          _ -> [MastLet ct i ty mInit']

mastString, mastInteger :: MastTy
mastString  = MastTyCon (Name "string")  []
mastInteger = MastTyCon (Name "integer") []
```

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

Note: specialization handles `integer` automatically as a nullary type
constructor — no changes to `Specialise.hs`.

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
        let x : integer = wordToInteger(42);
        let y : integer = integerAdd(x, wordToInteger(8));
        return wordFromInteger(integerMul(y, wordToInteger(2)));
    }
}
```

Expected: folds to constant `100`.

### Step 5 — `Num.fromInteger` in stdlib (separate)

- Add `fromInteger :: integer -> a` to `Num` class and `word`/`uint256`
  instances in `std/NumLib.solc`
- `word:Num` delegates to the builtin `wordFromInteger`
- Test: `test/examples/comptime/integer-from-integer.solc`

### Step 6 — Literal desugaring pass (separate)

The Zig-style desugaring pass: `tcLit` is changed to return `integer`, and the
pass inserts `wordFromInteger`/`fromInteger` at runtime use sites (not at
literal sites).  Described in `comptime-for-core.md`.  When complete, explicit
`wordToInteger(42)` at literal sites in user code is no longer needed.

---

## Known Gaps and Future Work

| Gap | Impact | Resolution |
|---|---|---|
| `let x : integer = 42` (bare literal) | Requires `wordToInteger(42)` | Literal desugaring pass (Step 6) |
| `integerAdd(42, 3)` with bare literals | Requires `wordToInteger` wrappers | Literal desugaring pass (Step 6) |
| `instance integer : Add/Sub/Mul` | Cannot write `n + m` for `integer` | Std instances; depends on Step 6 |
| `instance integer : Eq/Ord` | Use `integerLt`/`integerEq` in the interim | Std instances after Step 6 |
| `let x = 3` (unannotated) | Rejected for now | Future: default to `word` |
| `let x : uint256 = 3` | Requires `fromWord(3)` | Step 5 + literal desugaring pass |
| `integer` binding without `comptime` | EmitHull panic if not folded | Enforce `comptime` in SAIL/MAST checkers |
| `typeOfMastExp` for `integer` literals | Returns `word` — unreliable | Carry type tag in `MastLit` (AST change) |
