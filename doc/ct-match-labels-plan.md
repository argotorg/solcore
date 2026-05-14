# Implementation Plan: Comptime Expression Match Labels

## Goal

Allow comptime expressions as match case labels on numeric types:

```solidity
match x {
  | comptime 0                   => return 0;
  | comptime two + 0             => return 2;
  | comptime toWord(uint256(42)) => return 42;
  | other                        => return other;
}
```

Labels are prefixed with `comptime` to disambiguate from pattern variables.
They are evaluated at compile time by MastEval. After evaluation they become
literal patterns and flow through the existing pipeline unchanged. EmitHull
never sees an unevaluated expression label.

---

## Pipeline flow

```
Parser → AST (PExp) → Name resolution → Type check → DecisionTree (AtomicSwitch)
       → Specialise (specPat) → Mast (MastPExp) → MastEval evaluates → MastPLit
       → EmitHull (unchanged)
```

Key invariant: **MastPExp patterns must not reach EmitHull**. MastEval is
responsible for evaluating all expression labels to literals, and must report a
compile error if it cannot.

---

## Changes by module

### 1. `src/Solcore/Frontend/Syntax/SyntaxTree.hs` *(not in original plan)*

Add `PExp Exp` to the **surface** `Pat` type (the parser produces this):

```haskell
data Pat
  = Pat Name [Pat]
  | PWildcard
  | PLit Literal
  | PExp Exp        -- new
```

There are two `Pat` types: `SyntaxTree.Pat` (surface, produced by the parser)
and `Stmt.Pat a` (elaborated, parameterised by identifier type). Both need
`PExp`. The original plan only mentioned `Stmt.Pat`.

### 2. `src/Solcore/Frontend/Syntax/Stmt.hs`

Add `PExp (Exp a)` to `Pat a`:

```haskell
data Pat a
  = PVar a
  | PCon a [Pat a]
  | PWildcard
  | PLit Literal
  | PExp (Exp a)    -- comptime expression label (numeric matches only)
```

### 3. `src/Solcore/Frontend/Parser/SolcoreParser.y`

Use the `comptime` keyword as a prefix (already a keyword):

```
Pattern : ...
        | 'comptime' Expr  { PExp $2 }
```

**Deviation from plan**: the plan proposed restricting expression label syntax
to function calls, parenthesised expressions, and arithmetic to avoid ambiguity.
The `comptime` prefix is simpler and more self-documenting — no grammar
ambiguity at all.

### 4. Pretty printers

- `src/Solcore/Frontend/Pretty/SolcorePretty.hs`: `ppr (PExp e) = ppr e`
- `src/Solcore/Frontend/Pretty/TreePretty.hs`: same

### 5. `src/Solcore/Frontend/Syntax/NameResolution.hs`

```haskell
resolve (S.PExp e) = PExp <$> resolve e
```

### 6. `src/Solcore/Frontend/TypeInference/TcMonad.hs` *(not in original plan)*

Added `isNumericTy :: Ty -> TcM Bool`:

```haskell
isNumericTy ty
  | ty == word = pure True
  | TyCon n [] <- ty = do
      mti <- maybeAskTypeInfo n
      case mti of
        Just (TypeInfo _ [con] _) -> do
          (Constr _ fields, _) <- constrsFromEnv con
          pure (fields == [word])
        _ -> pure False
  | otherwise = pure False
```

A type is numeric if it is `word`, or an algebraic type with exactly one
constructor taking exactly one `word` argument (e.g. `uint256`).

### 7. `src/Solcore/Frontend/TypeInference/TcStmt.hs`

Add a case to `tcPat`:

```haskell
tcPat t' (PExp e) = do
  (e', _ps, t) <- tcExp e
  s <- unify t t'
  numericOk <- isNumericTy (apply s t)
  unless numericOk $
    tcmError $ "expression match label must have a numeric type, got: " ++ pretty (apply s t)
  withCurrentSubst (PExp (apply s e'), apply s t, [])
```

**Known gap**: predicates `_ps` from `tcExp` are discarded. The `tcPat`
signature returns `[(Name, Scheme)]` (variable bindings), not `[Pred]`. Changing
the signature would require updating many call sites. In practice, comptime
expression labels have concrete numeric types resolved by unification with the
scrutinee, so type class constraints are satisfied implicitly. A fuller
implementation would thread `_ps` upward.

### 8. `src/Solcore/Frontend/TypeInference/Erase.hs`

```haskell
erase (PExp e) = PExp (erase e)
```

### 9. `src/Solcore/Desugarer/IfDesugarer.hs`

```haskell
desugarBoolPat (PExp e) = PExp e
```

### 10. `src/Solcore/Desugarer/DecisionTreeCompiler.hs`

#### New types

```haskell
type AtomicPat = Either Literal (Exp Id)
-- Left  = literal (already evaluated)
-- Right = comptime expression (evaluated post-specialization)

data DecisionTree
  = ...
  | AtomicSwitch Occurrence [(AtomicPat, DecisionTree)] (Maybe DecisionTree)
  -- replaces LitSwitch
```

#### New/replaced functions

| Old | New | Notes |
|---|---|---|
| `patHeadLit` | `patHeadAtomic` | Returns `Maybe AtomicPat` |
| `buildLitSwitch` | `buildAtomicSwitch` | Handles mixed `PLit`+`PExp` columns |
| `specializeLit` | `specializeAtomic` | Uses `AtomicPat` equality |
| `litSpecializedBoundActs` | `atomicSpecializedBoundActs` | — |
| `litBranchToEqn` | `atomicBranchToEqn` | Emits `PLit` or `PExp` |

#### PANIC sites fixed

Added `PExp _ -> Nothing` / `PExp _ -> []` / `PExp _ -> False` in:
- `specRow` (constructor specialization)
- `defaultMatrix`
- `specializedBoundActs`

#### `isUseful` (redundancy checking)

```haskell
PExp _ -> pure True  -- conservative: always considered useful
```

#### `inhabitsM` *(not in original plan)*

Updated to use `patHeadAtomic` instead of `patHeadLit`.

### 11. `src/Solcore/Backend/Specialise.hs`

Two changes were needed — the plan only anticipated one:

**a) `toMastPat`** (as planned):

```haskell
toMastPat (PExp e) = MastPExp (toMastExp e)
```

**b) `specPat` in `specMatch`** *(not in original plan — discovered during implementation)*:

`PExp` expressions inside patterns were bypassing specialization. `toMastExp`
is a pure conversion that doesn't resolve type-class methods (e.g. `Add.add`
stays `Add.add` instead of becoming `addWord`). Fixed by adding `specPat` in
`specAlt`:

```haskell
specAlt (pat, body) = do
  body' <- specBody body
  pat'  <- mapM specPat =<< atCurrentSubst pat
  return (pat', body')

specPat :: Pat Id -> SM (Pat Id)
specPat (PExp e) = do
  ty <- atCurrentSubst (typeOfTcExp e)
  PExp <$> specExp e ty
specPat p = pure p
```

This runs `PExp` expressions through the full specialization machinery, resolving
type-class calls to their concrete implementations before Mast emission.

### 12. `src/Solcore/Backend/Mast.hs`

```haskell
data MastPat
  = ...
  | MastPExp MastExp  -- comptime expression label; must be evaluated by MastEval
```

Plus `ppr (MastPExp e) = ppr e`.

### 13. `src/Solcore/Backend/MastEval.hs`

Three sites needed updating — the plan mentioned two:

**a) `evalAlt`** (as planned):

```haskell
evalAlt env (pat, body) = do
  pat'       <- evalPat env pat
  (_, body') <- evalStmts env body
  pure (pat', body')

evalPat :: VEnv -> MastPat -> EvalM MastPat
evalPat env (MastPExp e) = do
  e' <- evalExp env e
  case e' of
    MastLit l -> pure (MastPLit l)
    _         -> error $ "comptime expression in match label could not be evaluated to a literal: " ++ show e'
evalPat _ pat = pure pat
```

**b) `evalFunBody` match case** *(not in original plan)*:

`evalFunBody` has its own match handling (static branch selection during
inlining) that bypasses `evalAlt`. Also needed `evalPat` calls there:

```haskell
MastMatch scrut alts -> do
  scrut' <- evalExp env scrut
  alts'  <- mapM (\(p, b) -> (,b) <$> evalPat env p) alts
  case matchAlts env scrut' alts' of ...
```

**c) Guards** (as planned):

- `findLitMatch`: `MastPExp _ -> error "PANIC: ..."`
- `bindPatterns`: `MastPExp _ -> error "PANIC: ..."` *(not in original plan)*

### 14. `src/Solcore/Backend/EmitHull.hs`

Guard added (as planned):

```haskell
emitWordAlt _ (MastPExp _, _) = errorsEM ["PANIC: MastPExp reached EmitHull ..."]
```

---

## Summary of touched files

| File | Nature of change |
|---|---|
| `Syntax/SyntaxTree.hs` | Add `PExp` to surface `Pat` *(missed in plan)* |
| `Syntax/Stmt.hs` | Add `PExp` to elaborated `Pat a` |
| `Parser/SolcoreParser.y` | `'comptime' Expr → PExp` |
| `Pretty/SolcorePretty.hs` | `ppr (PExp e)` |
| `Pretty/TreePretty.hs` | `ppr (PExp e)` |
| `Syntax/NameResolution.hs` | `resolve (PExp e)` |
| `TypeInference/TcMonad.hs` | `isNumericTy` predicate *(not in plan)* |
| `TypeInference/TcStmt.hs` | `tcPat` for `PExp` + numeric type check |
| `TypeInference/Erase.hs` | `erase (PExp e)` |
| `Desugarer/IfDesugarer.hs` | `desugarBoolPat (PExp e)` |
| `Desugarer/DecisionTreeCompiler.hs` | `AtomicSwitch`, `AtomicPat`, all related functions, PANIC fixes, `inhabitsM` |
| `Backend/Specialise.hs` | `toMastPat (PExp e)` + `specPat` in `specMatch` *(missed in plan)* |
| `Backend/Mast.hs` | `MastPExp` in `MastPat` |
| `Backend/MastEval.hs` | `evalPat`, `evalFunBody` fix *(missed in plan)*, `findLitMatch` + `bindPatterns` guards |
| `Backend/EmitHull.hs` | Guard for `MastPExp` |
| `test/MatchCompilerTests.hs` | Update `LitSwitch` → `AtomicSwitch`, `branchLits` |
| `test/examples/comptime/match_labels.solc` | End-to-end test |

---

## What the plan got wrong or missed

### Missed: `SyntaxTree.Pat`

The plan said to add `PExp` to `Stmt.Pat a`. There is a second `Pat` in
`SyntaxTree.hs` (the surface AST produced by the parser). Both needed `PExp`.

### Missed: `specPat` in specialization

The plan said `toMastPat (PExp e) = MastPExp (toMastExp e)`. This is correct as
far as it goes, but `toMastExp` is a pure structural conversion that doesn't
specialize type-class calls. `Add.add` would survive into Mast unresolved, and
MastEval couldn't evaluate `Add.add(2, 0)` since it wasn't in `pureFuns`.

The fix — running `PExp` expressions through `specExp` in `specMatch` — was the
most significant unplanned change.

### Missed: `evalFunBody` match case

The plan said "after `evalAlt` converts `MastPExp` to `MastPLit`, the existing
`findLitMatch` handles everything." This is only true for the `evalStmt` path.
`evalFunBody` has a separate match handling path (for static branch selection
during inlining) that also needs `evalPat` called on patterns.

### Missed: `bindPatterns` guard

The plan listed `findLitMatch` as needing a `MastPExp` guard; `bindPatterns`
(used for constructor matching) also needs one.

### Missed: `inhabitsM` update

`inhabitsM` in the decision tree compiler uses `patHeadLit`; this needed
updating to `patHeadAtomic`.

### Deviation: parser syntax

The plan proposed restricting expression label syntax structurally (function
calls, parenthesised expressions, arithmetic only). Instead, the `comptime`
keyword prefix was used. Simpler, zero ambiguity, more readable.

### Known gap: predicate propagation in `tcPat`

The plan flagged this as a risk. The implementation drops predicates from
`tcExp` with a comment. Works in practice because comptime expressions on numeric
types resolve constraints by unification. Should be revisited if type-class
overloaded comptime expressions cause issues.

---

## Out of scope

- Comptime validation at the type-checking phase (Option C Phase 2 from
  `comptime-design.md`)
- Expression labels on non-numeric types
- Detecting collision between expression labels that evaluate to the same value
  (could be added as a MastEval warning later)
- Predicate propagation from `PExp` in `tcPat`
