# Comptime Analysis: Design Options

> **Status: implemented, via Option C (hybrid).**  Both stages are in the tree:
> the late verifier is `Solcore.Backend.ComptimeCheck` and the early classifier is
> `Solcore.Frontend.ComptimeCheck`.  Comptime expression match labels, comptime
> results, comptime `let` bindings, and comptime annotations on class methods all
> work.  Sections 1–2 describe the shipped design; sections 3–6 are kept as the
> record of the options weighed, and section 7 records how the open questions were
> answered.  For the surrounding pipeline see
> [`architecture.md`](architecture.md); for the comptime-only types and the Yul
> interpreter see [`comptime-integer.md`](comptime-integer.md),
> [`comptime-string.md`](comptime-string.md), and
> [`comptime-asm.md`](comptime-asm.md).

## 1. Background

### What comptime means

A value marked `comptime` **must** be computable at compile time. If the compiler
cannot establish this, it must report an error. In most cases the compiler should
also evaluate the expression at compile time.

### Current state

The `comptime` keyword is parsed and stored as a `Bool` flag in `Param` constructors
throughout the typed AST, and preserved through specialization: `toMastParam`
(`Specialise.hs:1170`) carries it into `MastParam.mastParamComptime`, so both the
frontend and the backend checkers can see it.

### Relevant pipeline ordering

```
 Pass                              AST type         Phase
 ───────────────────────────────── ──────────────── ────────────
 Name resolution                   CompUnit Name    Untyped
   (numeric ops → Call Add.add)
 ...
 Type checking                     CompUnit Id      Typed
 Early comptime check              CompUnit Id      Typed
 Array literal desugaring          CompUnit Id      Typed
 If/Bool desugaring                CompUnit Id      Typed
 Match compilation                 CompUnit Id      Typed
 Specialization                    MastCompUnit     Monomorphic
 Partial evaluation (MastEval)     MastCompUnit     Monomorphic
 Dead code elimination             MastCompUnit     Monomorphic
 Late comptime check               MastCompUnit     Monomorphic
 Hull emission                     [Hull.Object]    Lowered
```

Key fact: **match compilation runs before specialization**.

Numeric operators (`+`, `-`, `*`) are desugared to overloaded function calls
(`Add.add`, `Sub.sub`, `Mul.mul`) at name resolution. By the time
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

Expression labels need a new pattern variant, since they are fundamentally
different from structural patterns — they don't bind variables, they compute
values. Implemented as `PExp` in the frontend (`Frontend/Syntax/Stmt.hs`) and
`MastPExp` in Mast (`Backend/Mast.hs:156`), written with a `comptime` keyword in
front of the label:

```solc
match selector {
  | comptime keccakLit("transfer(address,uint256)") => return 1;
  | _                                               => return 0;
}
```

An important constraint: **expression labels may reference variables from outer
scope** (e.g., `match x { | y + 1 => ... }` where `y` is a local). Patterns
normally *bind* variables; expression labels *use* them. This distinction must
be clear in the AST.

### When to evaluate

Expression labels pass through match compilation unevaluated, survive through
specialization into Mast, and are evaluated post-specialization by MastEval:
`evalPat` (`MastEval.hs:442`) reduces a `MastPExp` to a `MastPLit`. A label that
does not reduce to a literal is a hard failure there, and reaching Hull emission
still wrapped in `MastPExp` is a panic (`EmitHull.hs:503`). Because the reduction
happens before emission, the backend only ever sees ordinary literal patterns and
needs no special handling — no if-else chain is required.

### Scope restriction

Expression labels are restricted to **monomorphic numeric types** (primarily
`word`, potentially `uint8` etc.). This avoids overloading complications —
after specialization, all operations are concrete and evaluable. The restriction is
enforced by `tcPat'` (`TcStmt.hs:317`), which unifies the label's type with the
scrutinee's and then rejects a non-numeric result.

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
5. Yul backend handles expression labels via if-else chains — *not needed in the
   end: MastEval reduces every label to a literal, so the backend sees ordinary
   literal patterns (see Section 2)*

**Stage 2 (early classification, added later):**
6. Add comptime annotations to function result types and class methods
7. Conservative comptime classifier after type checking
8. Early error reporting for obvious violations

---

## 4. Common implementation elements (all options)

These changes are needed regardless of which option is chosen. **All seven have
landed** — items 5–7, listed below as Option A / C-Phase-2 work, came in with the
early classifier:

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

**Option C (Hybrid), starting with the Option B foundation.** Both stages are now
implemented: `Solcore.Backend.ComptimeCheck` verifies the monomorphic program after
partial evaluation, and `Solcore.Frontend.ComptimeCheck` classifies the typed AST
beforehand (`CTComptime` / `CTRuntime` / `CTDeferred`, deferring to the late check
whenever it cannot decide) and enforces that a declared-comptime binding is
immutable — including against assignment from inside an `assembly` block.

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

## 7. Questions, as answered

1. **Syntax for comptime results**: `-> comptime word`, as proposed. Class methods
   take the same annotations on parameters and results
   (`function fromInteger(comptime x : integer) -> comptime a;`, `std/std.solc:517`).
2. **Syntax for numeric switch**: `match` is reused. An expression label is written
   with a leading `comptime` keyword, which keeps it visibly distinct from a
   variable-binding pattern.
3. **Comptime let bindings**: the annotation goes on the type —
   `let x : comptime word = e`.
4. **Is comptime inferred for function results?** No. A result is comptime only when
   annotated. The *classifier* infers comptime-ness of expressions, and the frontend
   yields `CTDeferred` rather than guessing whenever it cannot decide, but a function's
   contract is always explicit.
5. **Interaction with assembly blocks**: an assembly block is *not* definitionally
   runtime. MastEval carries a Yul interpreter (see
   [`comptime-asm.md`](comptime-asm.md)), so a block it can interpret folds like any
   other expression; one it cannot — anything touching storage, calldata, or the
   outside world — makes the enclosing expression runtime. `ct_asm_ret.solc` pins the
   rejection of `-> comptime word` on a body that does `sload`, and `ct_asm_mem.solc`
   pins the folding case. Assembly does have one special rule, in the opposite
   direction: because a Yul block assigns to enclosing variables by name, the frontend
   check rejects assignments to comptime bindings from inside a block, exactly as it
   does for SAIL assignments.
6. **Recursive comptime functions**: yes, bounded by MastEval's fuel (`--pe-fuel N`,
   `fib3.solc` and `integer-fib.solc`). Exhausting the budget is reported as
   `warning[SC0401]`, which obeys `--warnings` and escalates under `--warnings deny`;
   the annotation that consequently failed to discharge is then rejected by the late
   check.
7. **Expression label collision**: (a) — the first matching branch wins, silently.
   Labels reduce to literals during partial evaluation, so a collision *could* be
   detected there by comparing the reduced literals, but nothing does so today. The
   worst outcome is a dead branch, not a wrong one.

### Still open

- **MAST-level rejections carry no source span.** Mast has no source locations, so a
  violation the frontend classifier deferred is reported against an arbitrary position
  in `std/opcodes.solc` rather than the user's code. The frontend check exists partly to
  keep the common cases away from this path, but it does not cover all of them.
- **Comptime-only types are not covered by the immutability rule.** `ctDeclared` is
  false for an `isComptimeOnlyTy` parameter by design, so `function f(s : string) { s =
  "x"; }` is caught only much later, by EmitHull's "comptime value not eliminated"
  guard. Whether the frontend rule should extend to them is undecided.
