# Comptime Analysis: Design Options

## 1. Background

### What comptime means

A value marked `comptime` **must** be computable at compile time. If the compiler
cannot establish this, it must report an error. In most cases the compiler should
also evaluate the expression at compile time.

### Current state

The `comptime` keyword is parsed and stored as a `Bool` flag in `Param` constructors
throughout the typed AST. It is currently ignored by all later phases and lost during
specialization (the `toMastParam` conversion discards it).

### Relevant pipeline ordering

```
 Step  Pass                          AST type         Phase
 ───── ───────────────────────────── ──────────────── ─────────
  2    Name resolution               CompUnit Name    Untyped
       (numeric ops → Call Add.add)
  ...
  9    Type checking                  CompUnit Id      Typed
 10    If/Bool desugaring             CompUnit Id      Typed
 11    Match compilation              CompUnit Id      Typed
 12    Specialization                 MastCompUnit     Monomorphic
 13    Partial evaluation (MastEval)  MastCompUnit     Monomorphic
 14    Dead code elimination          MastCompUnit     Monomorphic
 15    Hull emission                  HullCompUnit     Lowered
```

Key fact: **match compilation (step 11) runs before specialization (step 12)**.

Numeric operators (`+`, `-`, `*`) are desugared to overloaded function calls
(`Add.add`, `Sub.sub`, `Mul.mul`) at name resolution (step 2). By the time
type checking runs, `2 + 2` is `Call Add.add [Lit 2, Lit 2]` — a call to a
type-class method.

---

## 2. Expression labels in numeric switches

### Can expression labels survive match compilation unevaluated?

**Yes.** The match compiler does not need to evaluate expression labels. It uses
literal equality (`l == lit`) for matrix specialization — deciding which pattern
rows belong to which branch. Expression labels can be compared syntactically
rather than semantically. The consequences of syntactic-only comparison:

- **Two expressions evaluating to the same value** (e.g., `2+2` and `1+3`):
  Treated as distinct branches. One becomes unreachable after evaluation.
  Harmless — just suboptimal code.
- **Redundancy checking**: Cannot detect that `2+2` and `4` overlap. Acceptable
  limitation — redundancy is already imprecise for numeric types (can't
  enumerate all 2^256 word values).
- **Exhaustiveness**: Numeric switches are never exhaustive, so there is always
  a default branch. No problem.
- **Correctness**: The generated decision tree is correct. Worst case is dead
  branches, not wrong branches.

### Representation

Expression labels need a new pattern variant (e.g., `PExp (Exp Id)`) or an
extension to `Literal`, since they are fundamentally different from structural
patterns — they don't bind variables, they compute values.

An important constraint: **expression labels may reference variables from outer
scope** (e.g., `match x { | y + 1 => ... }` where `y` is a local). Patterns
normally *bind* variables; expression labels *use* them. This distinction must
be clear in the AST.

### When to evaluate

Expression labels pass through match compilation unevaluated, survive through
specialization into Mast, and are evaluated post-specialization (by MastEval
or a dedicated comptime evaluator). The Yul backend emits if-else chains
for expression labels rather than Yul `switch` statements (which require
literal case values).

### Scope restriction

Expression labels are restricted to **monomorphic numeric types** (primarily
`word`, potentially `uint8` etc.). This avoids overloading complications —
after specialization, all operations are concrete and evaluable.

---

## 3. Design Options

### Option A: Early analysis (at/after type checking)

Comptime-ness is treated as a **type qualifier** — a property tracked alongside
types during type inference, propagated through signatures, and checked at the
typed-AST level.

#### How it works

Add a comptime qualifier to function signatures and type class method declarations:

```
class a:Eval {
  function eval(comptime x : a) -> comptime word;
}

function f(comptime x : a) -> comptime word [Eval a] {
  return eval(x);
}
```

The comptime checker runs on `CompUnit Id` (typed AST), after type checking.
It classifies each expression as comptime or runtime using these rules:

| Expression form              | Comptime if                                        |
|------------------------------|----------------------------------------------------|
| `Lit n`                      | Always                                             |
| `Var x`                      | `x` is a comptime parameter or comptime-bound var  |
| `Call f args`                | `f`'s signature says result is comptime, given      |
|                              | which args are comptime; AND those args are comptime|
| `Con k args`                 | All `args` are comptime                            |
| `let x = comptime e; body`  | `e` is comptime; `x` is comptime in `body`         |

For type-class methods, the class declaration specifies which parameters and
results are comptime. Instance implementations must satisfy these contracts.

#### The overloading complication

At the typed-AST level, `2 + 2` is `Call Add.add [Lit 2, Lit 2]` with a resolved
type-class constraint. The checker can *classify* this as comptime (if `Add.add`'s
class signature says comptime inputs yield comptime output). But to *evaluate*
it, the checker needs to resolve which instance applies and inline the method
body — a mini-evaluator that understands instance resolution.

This is doable but non-trivial. However, for switch labels the early evaluation
is not strictly necessary (see Section 2 — labels can survive unevaluated).

#### Advantages

- Comptime is a **semantic contract**: part of the interface, checkable per-module
- Errors reported in terms of original source code
- Comptime annotations on class methods enable compositional reasoning
- Foundation for richer future features (comptime-dependent types, etc.)

#### Disadvantages

- Significant implementation effort in the type-checking phase
- Must handle polymorphism and instance resolution in the comptime checker
- Evaluating comptime expressions pre-specialization requires a mini-evaluator
  that understands overloading (if early evaluation is desired)
- Duplicates some work that MastEval does post-specialization

#### Implementation strategy

1. Extend `Param` and function signatures to carry comptime annotations on results
   (not just parameters)
2. Add comptime annotations to type class method declarations
3. Implement a comptime classification pass after type checking (new module,
   e.g. `Solcore.Frontend.TypeInference.TcComptime`)
4. Instance checker verifies comptime contracts

---

### Option B: Late analysis (after specialization)

Comptime analysis runs on the monomorphic `MastCompUnit`, as an extension of
or companion to MastEval.

#### How it works

1. Preserve the comptime flag through specialization into `MastParam`
2. After specialization, run a comptime classifier on Mast expressions
3. Verify that comptime-flagged parameters receive comptime arguments at all
   call sites
4. Use comptime information to guide more aggressive evaluation in MastEval

Classification rules on Mast (all types are concrete, no overloading):

| Expression form              | Comptime if                                      |
|------------------------------|--------------------------------------------------|
| `MastLit n`                  | Always                                           |
| `MastVar x`                  | `x` is comptime parameter or comptime-bound var  |
| `MastCall f args`            | `f` is pure AND all `args` are comptime           |
| `MastCon k args`             | All `args` are comptime                          |
| `MastCond c t e`             | All three are comptime                           |

Purity analysis already exists in MastEval (`computePureFuns`). Comptime
classification is a natural extension.

#### Switch labels

Expression labels in numeric switches are compatible with late analysis.
The labels pass through match compilation unevaluated (as expression patterns),
survive through specialization into Mast, and are evaluated post-specialization
when all types are concrete and operations are monomorphic. See Section 2 for
why the match compiler handles unevaluated labels correctly.

The Mast IR needs to support expression labels in match alternatives (currently
`MastPat` supports only `MastPLit Literal` for literal patterns). This requires
either extending `MastPat` with an expression variant or introducing a distinct
switch statement in Mast.

#### Advantages

- Simpler implementation: no polymorphism to handle
- Leverages existing MastEval infrastructure (purity analysis, constant folding)
- All types concrete — evaluation is straightforward
- Minimal changes to the type-checking phase
- Switch labels work (evaluated post-specialization)

#### Disadvantages

- Errors reported in terms of specialized code (worse diagnostics)
- Comptime is an implementation detail, not a semantic contract
- No compositional checking — must see whole program
- Cannot catch comptime errors until after expensive specialization
- Comptime annotations on class methods would be checked per-instance rather
  than per-class (weaker guarantee)

#### Implementation strategy

1. Add `mastParamComptime :: Bool` to `MastParam`
2. Preserve flag in `toMastParam` (Specialise.hs)
3. Add expression pattern variant to `Pat` / `MastPat` for switch labels
4. Match compiler treats expression labels as opaque distinct values
5. Add comptime classification pass after specialization (new module,
   e.g. `Solcore.Backend.ComptimeCheck`)
6. Extend MastEval to evaluate expression labels and use comptime information
   for aggressive evaluation
7. Yul backend emits if-else chains for expression labels

---

### Option C: Hybrid approach

Start with late analysis (Option B), later extend with early analysis for
richer comptime checking. The early check handles what it can; the late check
handles the rest.

#### How it works

**Phase 1 (implemented first): Late analysis**

Same as Option B. Full comptime verification on `MastCompUnit` after
specialization. Expression switch labels pass through unevaluated and are
resolved at this stage.

**Phase 2 (added later): Early classification**

A conservative comptime classifier on `CompUnit Id` (typed AST). Does not
evaluate expressions — only classifies them as comptime or runtime (or
deferred). Purpose: catch obvious errors early, enable comptime annotations
on class methods as semantic contracts.

| Expression form              | Classification                                    |
|------------------------------|---------------------------------------------------|
| `Lit n`                      | Comptime                                          |
| `Var x` (comptime param)     | Comptime                                          |
| `Var x` (comptime let-bound) | Comptime                                          |
| `Call f args` where `f` has  | Comptime if the relevant `args` are comptime      |
| comptime signature           |                                                   |
| Everything else              | **Deferred** (verified post-specialization)       |

The early phase classifies but does not evaluate. Evaluation always happens
post-specialization. This avoids the need for an early mini-evaluator that
understands instance resolution.

#### Advantages

- Incremental: start simple (Option B), add sophistication later
- Switch labels work from day one (via late evaluation)
- Early phase catches errors sooner when added
- Migration path toward full early analysis (Option A) if desired
- No duplicated evaluation logic — all evaluation in MastEval

#### Disadvantages

- Initially same limitations as Option B (no early error detection)
- When early phase is added, two analysis phases to maintain
- The boundary between early and late checking needs careful definition

#### Implementation strategy

**Stage 1 (Option B foundation):**
1. Preserve comptime flag through specialization into `MastParam`
2. Add expression patterns for switch labels (AST, parser, match compiler)
3. Comptime classification pass after specialization
4. MastEval evaluates expression labels and comptime expressions
5. Yul backend handles expression labels via if-else chains

**Stage 2 (early classification, added later):**
6. Add comptime annotations to function result types and class methods
7. Conservative comptime classifier after type checking
8. Early error reporting for obvious violations

---

## 4. Common implementation elements (all options)

These changes are needed regardless of which option is chosen:

1. **Preserve comptime flag into Mast**: `MastParam` gains a `mastParamComptime`
   field; `toMastParam` preserves it from `Param Id`.

2. **Expression pattern variant**: New pattern type for switch expression labels.
   Must be distinct from variable-binding patterns. Restricted to monomorphic
   numeric types (`word`, potentially `uint8` etc.).

3. **Parser changes for switch labels**: Allow expressions in numeric match/switch
   label positions.

4. **Comptime expression evaluator**: Post-specialization evaluation of comptime
   expressions in MastEval (all options need this).

Changes needed for Options A and C (Phase 2) but not Option B alone:

5. **Comptime annotations on function results**: Extend signature representation.

6. **Comptime annotations on class methods**: Class declarations specify comptime
   on method parameters and results.

7. **`let x = comptime e` syntax**: Parser and AST changes.

---

## 5. Diverging elements

| Aspect                     | Option A (Early)          | Option B (Late)           | Option C (Hybrid)           |
|----------------------------|---------------------------|---------------------------|-----------------------------|
| Switch labels              | Yes                       | Yes                       | Yes                         |
| Handles overloading        | Yes (instance resolution) | N/A (post-specialization) | Late: N/A; Early: partially |
| Polymorphic comptime sigs  | Yes                       | No (per-specialization)   | Added in Phase 2            |
| Implementation complexity  | High                      | Low-Medium                | Low-Medium → Medium         |
| Error quality              | Best                      | Late only                 | Late → Late + Early         |
| Compositional checking     | Yes                       | No                        | Added in Phase 2            |
| Changes to type checker    | Significant               | None                      | None → Minimal              |
| MastEval changes           | Minimal                   | Significant               | Significant → same          |
| Incremental delivery       | No (big bang)             | Yes                       | Yes (designed for it)       |

---

## 6. Recommended approach

**Option C (Hybrid), starting with the Option B foundation.**

Rationale:
- Start with late analysis: simplest implementation, handles all use cases
  including switch labels
- Expression labels pass through match compilation unevaluated — no pipeline
  restructuring needed
- Restrict expression labels to monomorphic numeric types (word, uint8, etc.)
- Later extend with early classification for better error messages and
  comptime-as-contract semantics
- Provides a migration path toward Option A if richer comptime is desired

---

## 7. Open questions

1. **Syntax for comptime results**: `-> comptime word` or a different notation?
2. **Syntax for numeric switch**: Reuse `match` with expression labels, or
   introduce a separate `switch` keyword?
3. **Comptime let bindings**: `let x = comptime e` or `comptime let x = e` or
   `let comptime x = e`?
4. **Should comptime be inferred for function results?** If all inputs are
   comptime and the function is pure, the result could be automatically comptime.
   Explicit annotation gives stronger contracts; inference reduces annotation
   burden.
5. **Interaction with assembly blocks**: Any expression containing inline
   assembly is definitionally not comptime. Should this be explicit?
6. **Recursive comptime functions**: Can a recursive function be comptime?
   Requires bounded recursion or a fuel limit to guarantee termination.
   MastEval already uses fuel — same mechanism could apply.
7. **Expression label collision**: Two expression labels evaluating to the same
   value at runtime. Options: (a) silently pick first matching branch,
   (b) emit a compiler warning if detected during evaluation, (c) treat as
   error. Since labels are comptime, collision can be detected at compile time
   after evaluation.
