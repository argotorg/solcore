# Comptime Integer Type: Design and Implementation

## Overview

`integer` is a compile-time-only type for unlimited-precision integer arithmetic.
It is the intended eventual type of all integer literals in Solcore.  All
implementation steps have been completed; bare integer literals are universally
polymorphic — they desugar to `Int.fromInteger(n)` calls and are resolved
to the appropriate concrete type at each use site.

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
| Literal syntax | `42` desugars to `Int.fromInteger(42)` (see below); resolved at each use site |

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
and is the single source of truth used by `MastEval.builtinPureFuns` and
`Specialise.comptimeBuiltins`.  Add new primitives to `integerPrimNames` and both
consumers update automatically.

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

`integerLt` and `integerEq` back the `instance integer : Ord` and
`instance integer : Eq` definitions in `std.solc`.

### Standard library: `fromInteger`

`fromInteger :: integer -> a [Num a]` is the overloaded version defined in
`std/NumLib.solc` as a separate step after the core PoC:

- `word:Num` delegates to the builtin `wordFromInteger`
- `uint256:Num` wraps `wordFromInteger` in `Typedef.abs`

This enables `let x : uint256 = Num.fromInteger(fib(wordToInteger(10)))`.

---

## The `Int` Class

The primitive class `Int` (in `Primitives.hs`, registered in `TcEnv.hs`)
provides overloaded coercion from `integer` literals to concrete numeric types:

```
class a : Int { fromInteger : integer -> a }
instance word    : Int  -- fromInteger = wordFromInteger (primitive)
instance integer : Int  -- fromInteger = identity        (primitive)
instance uint256 : Int  -- fromInteger = uint256(wordFromInteger(x)) (std.solc)
```

The `word` and `integer` instances are primitive (no source body).  Concrete
numeric types define their own `Int` instance in `std.solc`; `uint256` wraps
`wordFromInteger` in its constructor, so `let x : uint256 = 42` truncates the
literal mod 2^256 exactly like a `word` site.  `Int.fromInteger` is injected by
the desugaring pass around every integer literal, but it is also accessible to
user code directly (the class and method are registered in the name resolver's
`emptyEnv`).  Because `Int` is a reserved primitive class name, user programs
cannot define their own class named `Int`.

Note: `Int` method names are stored **qualified** in the primitive class's
`ClassInfo` (like `invokable`), so `verifySignatures` must not re-qualify them
when checking a source instance body — otherwise the method resolves to the
double-qualified `Int.Int.fromInteger`.

`Int.fromInteger` is in `integerPrimNames`, so it seeds `builtinPureFuns`
and is treated as a comptime builtin by the specializer.

### Specializer handling

`Specialise.specCall` has a dedicated case for `Int.fromInteger` that
runs before the generic `comptimeBuiltins` guard:

- result type `word` → rewrite to `wordFromInteger` (evaluated by MastEval)
- result type `integer` → keep as `Int.fromInteger` (identity, also
  evaluated by MastEval via `evalPrimitive`)

### MastEval handling

```haskell
evalPrimitive (QualName (Name "Int") "fromInteger") [x] = Just x
```

This covers the `integer -> integer` case (identity).  The `integer -> word`
case has already been rewritten to `wordFromInteger` by the specializer.

---

## Literals and the desugaring pass

### Implemented approach

`Solcore.Desugarer.IntLiteralDesugar.desugarIntLiterals` runs before type
inference on the untyped AST (`CompUnit Name`).  It uses SYB `everywhere` to
wrap every `Lit (IntLit n)` in **expression position** with a call to
`Int.fromInteger`:

| Source site | After desugaring |
|---|---|
| `let x : integer = 42` | `let x : integer = Int.fromInteger(42)` |
| `let x : word = 42` | `let x : word = Int.fromInteger(42)` |
| `integerAdd(1, 2)` | `integerAdd(Int.fromInteger(1), Int.fromInteger(2))` |
| `fib(10)` where `fib :: integer -> integer` | `fib(Int.fromInteger(10))` |

The type checker then resolves `Int.fromInteger(42)` using the constraint
`a : Int` against the expected type at each site.  `tcLit (IntLit _)`
returns `Prim.integer` (the literal's argument type inside the call).

`PLit (IntLit n)` patterns are **not** touched — they are not `Exp Name` values
and are handled separately by `tcPat` (see below).

### Pattern literal handling

`tcPat t' (PLit l@(IntLit _))` adopts the scrutinee type directly rather than
inserting a coercion:

- If the scrutinee type is numeric (`word`, `integer`, or a single-constructor
  newtype of `word` per `isNumericTy`), the pattern accepts that type.
- If the scrutinee type is still a metavariable (`Meta _`), it is defaulted to
  `word`.
- Otherwise a type error is reported.

**Known limitations of this approach:**

1. **`isNumericTy` is a closed whitelist** (`TcMonad.hs`).  It accepts `word`,
   `integer`, and algebraic types with exactly one constructor wrapping a single
   `word`.  Any numeric type not fitting this structural pattern (e.g. a
   multi-field newtype) would be rejected by `PLit` patterns even if semantically
   it should be fine.

2. **`Meta _` default to `word`** is a heuristic that can mask type errors when
   the scrutinee type is genuinely ambiguous rather than just deferred.

3. **`scrutineeType (Lit (IntLit _)) = pure word`** in `DecisionTreeCompiler.hs`
   hardcodes `word` as the type of a bare integer literal scrutinee.  After
   literal desugaring this case should only be reached by `PExp (Lit (IntLit _))`
   patterns (comptime expression labels), but the assumption is implicit.

The `PExp` pattern form (comptime expression labels) is documented separately in
`ct-match-labels-plan.md`; its `tcPat` case also uses `isNumericTy` and discards
predicates from `tcExp` (noted gap there too).

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
, QualName (Name "Int") "fromInteger"
```

`Int.fromInteger` is included so that functions calling it (e.g. anything
using integer literals) are recognised as pure and can be inlined by MastEval.

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
path.

`Specialise.hs` was extended with a dedicated `specCall` case for
`Int.fromInteger` (see the Int Class section above).  All other
integer primitives pass through via the `comptimeBuiltins` guard unchanged.

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

Each step leaves all existing tests passing.

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

### Step 6 — Polymorphic literal desugaring ✓

Replaced the typed `IntegerLiteralDesugar` pass (which inserted `wordToInteger`
coercions at known `integer`-typed sites) with a simpler untyped approach:

- Added the `Int` primitive class with `word:Int` and
  `integer:Int` instances (see `Primitives.hs`, `TcEnv.hs`).
- `Solcore.Desugarer.IntLiteralDesugar.desugarIntLiterals` wraps every
  `Lit (IntLit n)` in expression position with `Int.fromInteger(n)`
  using SYB, with no type information required.
- `tcLit (IntLit _)` now returns `Prim.integer` (the type of the literal
  argument inside the `Int.fromInteger` call).
- `tcPat t' (PLit l@(IntLit _))` was added to handle integer literal patterns
  by adopting the scrutinee type (see "Pattern literal handling" above).
- `Specialise.specCall` resolves `Int.fromInteger` to `wordFromInteger`
  or identity depending on the concrete result type.
- The "no desugaring" validity check (`noDesugarCalls=True`) skips `Int`
  constraints in `ambiguityCheck`, `checkEntails`, and `hnfEntails` to avoid
  false failures when lambda closure conversion is not run.

The old `IntegerLiteralDesugar` module has been deleted.

Tests: `integer-lit.solc`, `integer-lit-safe.solc`, `integer-lit-class.solc`,
`integer-lit-word-site.solc`, `integer-lit-poly.solc`, `integer-lit-cond.solc`,
`integer-lit-pat.solc`, `match_labels.solc`.

---

## Known Gaps and Future Work

| Gap | Impact | Resolution |
|---|---|---|
| ~~`let x : integer = 42` (bare literal)~~ | ~~Requires `wordToInteger(42)`~~ | ✓ Step 6 |
| ~~`integerAdd(42, 3)` with bare literals~~ | ~~Requires explicit wrappers~~ | ✓ Step 6 |
| `let x = 3` (unannotated) | Type variable left ambiguous by `Int.fromInteger` | Future: default to `word` |
| `let x : uint256 = 3` | No `uint256:Int` instance; use `Num.fromInteger(wordToInteger(3))` with std | Add `uint256:Int` instance or extend `Int` to numeric newtypes |
| ~~`instance integer : Add/Sub/Mul`~~ | ~~Cannot write `n + m` for `integer`~~ | ✓ Added to `std.solc` |
| ~~`instance integer : Eq/Ord`~~ | ~~Use `integerLt`/`integerEq` in the interim~~ | ✓ Added to `std.solc` |
| `integer` binding without `comptime` | EmitHull panic if not folded | Enforce `comptime` in SAIL/MAST checkers |
| `typeOfMastExp` for `integer` literals | Returns `word` — unreliable | Carry type tag in `MastLit` (AST change) |
| `isNumericTy` whitelist in `tcPat` | Integer literal patterns rejected for non-`word`-newtype numeric types | Extend `isNumericTy` or use `Int` constraint check instead |
| `Meta _` default to `word` in `tcPat` | Silently resolves ambiguous scrutinee type; can mask errors | Propagate ambiguity instead of defaulting |
| `scrutineeType (Lit (IntLit _))` hardcoded `word` | Stale assumption in `DecisionTreeCompiler`; safe now, fragile long-term | Update when bare `IntLit` scrutinees are no longer possible |
| Predicates discarded in `tcPat (PExp e)` | Type-class constraints from comptime expression labels not propagated | Thread `[Pred]` through `tcPat` signature (many call-site changes) |
