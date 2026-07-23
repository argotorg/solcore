# Comptime String Type: Design and Plan

> **Status: implemented.**  Steps 1–6 below are done; all 712 tests pass,
> including the four new `string-*` tests.  Line citations point at the
> post-merge source.

## Overview

`string` is a compile-time-only type for string literals and exact
compile-time string manipulation.  It mirrors the design of the comptime
[`integer`](comptime-integer.md) type: bare string literals are universally
polymorphic — they desugar to `Str.fromString(s)` calls and are resolved to the
appropriate concrete representation at each use site.

The crucial difference from `integer` is the runtime story.  An `integer`
literal converts to its runtime form (`word`) by a **pure comptime fold**
(`wordFromInteger` masks an `Integer`, both sides live in `IntLit`).  A `string`
has **no on-stack runtime form at all** — at runtime a string is an empty type.
To be usable at runtime it must be *materialized* into memory as a
`memory(string)`: a length word followed by the character bytes, padded to
32-byte slots.  Materialization is therefore runtime **code generation**, not a
fold, and is the one genuinely new mechanism this design adds over the integer
work.

### Mental model (read this first)

- A `string` value exists **only at compile time**.  String literals and the
  results of comptime ops (`concatLit`, …) are `string`.
- To use a string at runtime you must **explicitly convert** it to a runtime
  representation with `Str.fromString`.  The only such representation in this
  first cut is `memory(string)`.
- There is **no implicit coercion**.  `let s : string = …; return s;` where the
  function returns `memory(string)` is a **type error** — exactly as `integer`
  requires an explicit `wordFromInteger` rather than auto-coercing a variable.
  Write `return Str.fromString(s);`.
- The conversion's argument must fold to a literal **in the scope where
  `Str.fromString` is applied** (see [Materialization](#materialization-in-emithull)
  for why a `string`-taking helper function does not work without inlining).

### What already exists

- `string = TyCon "string" []` (`Primitives.hs`).
- `tcLit (StrLit _) = string` (`TcStmt.hs:308`),
  `typeOfTcExp (Lit (StrLit _)) = string` (`Specialise.hs:642`).
- `EmitHull.emitLit (StrLit _) = error "String literals not supported yet"`
  (`EmitHull.hs:220`) — so `string` is *already* de-facto comptime-only: a bare
  literal cannot reach runtime.  It only survives by being folded away or
  consumed by a special case.
- Comptime folding already works: `concatLit`/`strlenLit`/`keccakLit` are
  `std.solc` stubs that MastEval folds when their arguments are concrete
  `StrLit` (`MastEval.hs:435-446`).  So string ops compose at compile time and
  collapse to a single `StrLit`.
- The runtime representation `memory(string)` exists: `data memory(t) =
  memory(word)` (`std.solc:752`), a `Typedef` over a pointer.  `strlen` reads
  `mload(ptr)` (`std.solc:818`); ABIEncode/ABIDecode/CanStore all operate on
  `memory(string)`.
- The `revertLit` special case (`EmitHull.hs:285`) already turns
  `MastCall revertLit [StrLit s]` into a Hull `SRevert s` — the precedent the
  materializer follows.

### What is missing

There is no way to take a comptime `string` (a literal, or the folded result of
`concatLit`) and materialize it into a runtime `memory(string)`.  That is the
whole job of this work.

---

## The `Str` class

Direct analog of the primitive `Int` class.  Registered in `Primitives.hs` and
`TcEnv.hs`, the name reserved in `NameResolution.emptyEnv`.

```
class a : Str { fromString : string -> a }
instance string         : Str   -- fromString = identity (comptime)
instance memory(string) : Str   -- fromString = memStringFromLit (runtime materialization)
```

Because `fromString : string -> a` resolves by **result type**, each runtime
representation gets its own materialization strategy through its own instance,
with no change to the others.  This is the extension point for the data-section
form (below).

Each non-identity instance maps to a distinct primitive name that the backend
intercepts:

| Instance | `fromString` resolves to | Backend handling |
|---|---|---|
| `string : Str` | identity | literal survives as comptime `StrLit` |
| `memory(string) : Str` | `memStringFromLit : string -> memory(string)` | EmitHull: inline `mstore` materializer |
| `code(string) : Str` *(future)* | `codeStringFromLit : string -> code(string)` | EmitHull: Yul `data` section + `codecopy` |

`Str` is a reserved primitive class name; user programs cannot redefine it.

### Source instances

Beyond the built-in instances for `string` and `memory(string)`,
a program may declare its own `Str` instance for
its own type, and `Str.fromString` at that result type is specialised through the
ordinary resolution table (`Specialise.specCall` falls through for any ground
result type that is neither `string` nor `memory(string)`).  What such an
instance can *do* is limited by the function boundary described under
[Materialization](#materialization-in-emithull): inside the method body the
argument is a parameter, not a literal.  An instance whose body consumes the
string at comptime (`strlenLit`, `keccakLit`, …) folds away and works — see
`test/examples/comptime/string-user-instance.solc`.  An instance that tries to
*materialize* — `instance Error : Str { fromString(s) = Error.Msg(Str.fromString(s)) }`,
which would give `require(cond, "msg")` — does not: it reaches emission with a
live `string` parameter and is rejected by the `comptimeOnlyMastName` guard.
Lifting that needs literal-keyed specialisation (substituting comptime-only-typed
arguments into the specialised body); see gaps.

Note that `memory(string)` is a **compound** instance head
(`TyCon "memory" [TyCon "string" []]`), unlike the nullary `word`/`integer`
heads of the `Int` instances; instance selection unifies `a := memory(string)`.
This is a slightly richer instance than the integer precedent, not a literal
copy.

### Instance registration (correctness-critical)

Both instances are registered as **bodyless heads** in `primInstEnv`
(`TcEnv.hs:205`), exactly like the `Int` instances
(`[] :=> InCls intClassName word []`):

```haskell
( strClassName,
  [ [] :=> InCls strClassName string [],
    [] :=> InCls strClassName (TyCon (Name "memory") [string]) []
  ] )
```

They exist **only to satisfy constraint solving** during type checking.  The
actual rewrite happens in the dedicated `specCall` case (below), which keeps the
*original argument expression in place*.

It is tempting to instead write `instance memory(string):Str { function
fromString(s) { return memStringFromLit(s); } }` in `std.solc`.  **Do not** — it
reintroduces the function-boundary problem: inside the instance method `s` is a
runtime parameter, so the body specializes to `memStringFromLit(s_param)` with
`s_param` a `MastVar`, never a `StrLit`, and EmitHull's intercept (which matches
`[MastLit (StrLit _)]`) never fires.  The dedicated `specCall` rewrite avoids
this precisely because it operates on the call site's own arguments, where the
literal is still present.  This is the same reason the `Int` instances are
bodyless and handled in `specCall`.

`Str` is registered in `primClassEnv` as a one-method class info (mirror
`intInfo` at `TcEnv.hs:230`), and `fromStringEntry` is added to `primCtx`
alongside `fromIntegerEntry` (`TcEnv.hs:176`).

---

## Literals and the desugaring pass

`Solcore.Desugarer.StrLiteralDesugar.desugarStrLiterals` (a clone of
`IntLiteralDesugar`) runs before type inference on the untyped AST and wraps, in
**expression position**, with `Str.fromString(…)` using SYB `everywhere`:

- every `Lit (StrLit s)`, and
- every `concatLit(…)` call (matched by leaf name, so `std.concatLit` too).

`tcLit (StrLit _)` continues to return `string` (the argument type inside the
call).  The checker resolves each `Str.fromString(…)` against the expected type
at its site.

| Source site | Expected type | Resolves to |
|---|---|---|
| `revertLit("msg")` | `string` (revertLit's param) | identity → `StrLit` survives; EmitHull `SRevert` fires |
| `f("x")` where `f : memory(string) -> …` | `memory(string)` | `memStringFromLit` → runtime alloc + store |
| `let s : string = "abcd"` | `string` | identity → comptime `StrLit` |
| `"a" + "b"` evaluated at `string` | `string` | identity on both operands → `concatLit` folds |
| `return concatLit("a","b")` from a `memory(string)` fn | `memory(string)` | wrapped `concatLit` → `memStringFromLit(concatLit …)` → materialize |

Wrapping `concatLit` makes it effectively result-polymorphic
(`string -> string -> a [a:Str]`) **without** changing its declaration or adding
any type-checker coercion — it is the same untyped literal-wrapping mechanism.
So `let x : memory(string) = concatLit("a","b")` works directly (no manual
`Str.fromString`).  Because `concatLit`'s parameters are `string`, a wrapped
`concatLit` used as an **argument** to another `concatLit` resolves to identity,
so nested concatenations (`concatLit(concatLit(a,b),c)`) collapse to one literal
and materialize once.  `+` is *not* helped (it desugars to `Add.add` at the
surface; `concatLit` only appears inside `string:Add` after typing), so the
dedicated `concatLit` is the terse comptime-concat form; `+` still needs the
explicit `Str.fromString("a" + "b")`.

Trade-off: an **unannotated** `let s = concatLit("a","b")` becomes ambiguous
(`a : Str`), exactly like `let s = "literal"` already is — resolvable later by a
defaulting rule.

**Patterns are out of scope.**  `PLit (StrLit _)` patterns are not handled by
this work.  Matching a string literal against a `memory(string)` scrutinee is
runtime string comparison — a separate feature.  (A future comptime-only form,
matching a `string` scrutinee against literal labels, could be added but is not
planned here.)  `tcPat` keeps its existing behaviour; no new `StrLit` pattern
case is added.

The `noDesugarCalls` validity check (`TcSimplify.hs`) skips `Str` constraints in
`checkEntails`/`hnfEntails`/`ambiguityCheck`, exactly as it skips `Int`.

### `datasize`/`dataoffset`

These Yul primops take a `string` that names a Yul *object* (not character
data): `datasize`/`dataoffset` typed `string -> word` (`Primitives.hs:336-337`).
After desugaring, `datasize("runtime")` becomes
`datasize(Str.fromString("runtime"))`, which resolves to the `string` identity
instance and folds back to `datasize(StrLit "runtime")` — the form the Yul
translator already expects.  No special handling needed; mentioned only because
their argument is a name, not materializable data.

---

## The comptime → runtime boundary

Concatenation and other string ops live in the `string` (identity) domain;
MastEval folds them to a single `StrLit`; only the value passed to
`Str.fromString` at a runtime representation is materialized.

### Worked example: `+` then materialize

`+` on strings uses `instance string:Add` (`std.solc:934`), whose body is
`concatLit` — so this form requires `import std`.

Source (function returns `memory(string)`):
```solidity
return Str.fromString("a" + "b");
```

After `StrLiteralDesugar` (every literal wrapped):
```
return Str.fromString( Add.add( Str.fromString("a"), Str.fromString("b") ) );
```

Type resolution, outside-in:
1. The return expects `memory(string)`, so the **outer** `Str.fromString`
   resolves at `memory(string)` → `memStringFromLit`.  Its argument is forced to
   `string`.
2. `Add.add` must therefore produce `string` → `instance string:Add`, forcing
   both operands to `string`.
3. The **inner** `Str.fromString("a")` / `Str.fromString("b")` resolve at
   `string` → identity.

MastEval then folds: `concatLit("a","b") → StrLit "ab"`, leaving
`memStringFromLit(StrLit "ab")`, which EmitHull materializes.

By the time `memStringFromLit` is reached its argument is always a concrete
`StrLit` — whether it came from source or from a comptime op.  A `string` value
that is *not* statically known cannot reach runtime; that is the comptime-only
invariant (the same one `integer` relies on), enforced by the EmitHull guard
plus the `emitLit (StrLit _) = error` backstop.

---

## Materialization in EmitHull

Chosen home: **EmitHull**.  It owns the object body, already has
`SFunction` + `SAssembly [YulStmt]` (`Hull.hs:58,64`), and follows the
`revertLit` intercept precedent.  Materialization is runtime codegen from a
comptime value, which is EmitHull-shaped.

### Why the argument must be a local literal

EmitHull intercepts
`MastCall (MastId "memStringFromLit" _) [MastLit (StrLit s)]`.  The argument has
to be a literal *at that point*.  This holds when `Str.fromString` is applied to:

- a literal directly (`Str.fromString("x")`), or
- a local `string` variable whose comptime `let` folds to a literal and is
  substituted by dead-let elimination in the same function scope.

It does **not** hold across a function boundary: a helper
`toMemory(s : string) { return Str.fromString(s); }` specializes to
`memStringFromLit(s_param)` with `s_param` a `MastVar`, never a `StrLit`.
`memStringFromLit` is impure, so MastEval will not inline the helper (unlike
pure helpers such as `Num.fromInteger`), and specialization is by type, not by
literal value.  A readable `toMemory` helper would require an "always inline"
mechanism, which is out of scope.  For now, apply `Str.fromString` directly
where the literal is in scope.

### Mechanism (inline `mstore`, first cut)

On intercept:

1. Compute at compile time: `len = byteLength(s)` (UTF-8, matching the existing
   `strlenLit` encoding in `MastEval.hs:437`), the char words `w0, w1, …` (each
   32 bytes, last right-padded with zero bytes), and
   `total = 32 * (1 + ceil(len / 32))`.
2. Register one generated function **per distinct literal**, deduplicated by content
   (a Map keyed on the literal; the suffix is an assigned index), named `__strlit_<n>`.  It is **self-contained** — it bumps
   the free-memory pointer itself and calls nothing external (the materializer
   is injected *after* specialization/tree-shaking, so it must not depend on
   `allocate_memory` or other std functions, which may have been pruned):

   ```
   // emitted as a Hull SFunction; memory(string) is a Typedef over word,
   // so at this level it is just a word (the pointer).
   function __strlit_<n>() -> word {
       let p;
       assembly {
           p := mload(0x40)                 // free pointer
           mstore(p, <len>)                 // length slot
           mstore(add(p, 32), <w0>)
           mstore(add(p, 64), <w1>)
           // ...
           mstore(0x40, add(p, <total>))    // bump free pointer
       }
       return p;                            // the memory(string) representation
   }
   ```
   `<len>`, `<wi>`, and `<total>` are compile-time constants baked into the Yul.
3. Rewrite the call site to `ECall "__strlit_<n>" []`.  The surrounding
   context already expects the `memory(string)` representation, which is exactly
   this word pointer (`Typedef` is representationally identity).
4. Append the accumulated `SFunction`s to the emitted object.

### Concrete Hull shape (verified mechanically sound)

The generated function is built directly as Hull (`p` is a Hull local; the asm
references it by name):

```haskell
Hull.SFunction "__strlit_<n>" [] Hull.TWord
  [ Hull.SAlloc "p" Hull.TWord
  , Hull.SAssembly
      [ YAssign ["p"] (YCall "mload" [YLit (YulNumber 0x40)])
      , YExp (YCall "mstore" [YIdent "p", YLit (YulNumber <len>)])
      , YExp (YCall "mstore" [YCall "add" [YIdent "p", YLit (YulNumber 32)],  YLit (YulNumber <w0>)])
      , YExp (YCall "mstore" [YCall "add" [YIdent "p", YLit (YulNumber 64)],  YLit (YulNumber <w1>)])
      -- ...
      , YExp (YCall "mstore" [YLit (YulNumber 0x40), YCall "add" [YIdent "p", YLit (YulNumber <total>)]])
      ]
  , Hull.SReturn (Hull.EVar "p")
  ]
```

This is sound because `genStmt (SAssembly …)` substitutes Hull variable names
into the asm via `getVarEnv` (`yule/Translate.hs:109-156`): the `p` written by
`SAlloc` is rewritten to its real Yul name everywhere it appears in the block.
So there is **no** Hull-vs-Yul naming hazard, and no need to fall back to inline
emission.

### EmitHull plumbing

`EM = StateT EcState IO`.  `EcState` gained `ecStrLits :: Map String String`
(`EmitHull.hs:39`), mapping literal content → generated function name, for
dedup.  `registerStrLit` (the intercept in `emitExp`) looks the literal up,
reusing the name if present or assigning `"__strlit_" ++ show (Map.size m)`, and
returns `(Hull.ECall fn [], [])`.  `emitContract` resets `ecStrLits` at entry,
builds `runtimeBody`, then reads `ecStrLits`, generates one `SFunction` per
entry, and **prepends** them to the object body (`Hull.Object cname (strLitFuns
++ runtimeBody) []`).

Limitation (first cut): the generated allocators are emitted into the **runtime**
object only.  A string literal used in constructor/deploy code would call an
allocator that lives in the runtime object — a separate Yul object — and not
resolve.  String literals in constructor code are therefore not yet supported
(see gaps).

Dedup-by-content means N identical literals share one function (a separate
function per *distinct* literal).

### Length boundaries to cover

- `len == 0`           → one slot (length only), `total = 32`.
- `0 < len < 32`       → two slots (length + one partial char word).
- `len == 32`          → two slots (length + one full char word).
- `len > 32`           → length + `ceil(len/32)` char words.

### Future: data section via `code(string)`

For large strings the inline `mstore` sequence bloats code.  Solidity instead
places literal bytes in the contract's data section and `codecopy`s them into
memory on demand.  The primops `datasize`/`dataoffset` and
`YulData`/`InnerData` (`Yul.hs:13-17`) already exist.

This slots in additively as a new instance `code(string) : Str` →
`codeStringFromLit : string -> code(string)`, where `code(string)` is a handle
to bytes living in the data section.  Its `fromString` registers a
`data "__strdata_<hash>" hex"…"` object and returns the handle; a conversion
(`code(string) -> memory(string)` via `codecopy`) materializes on demand.  Full
semantics of `code(a)` are out of scope for the first cut; the design keeps the
strategy keyed on target type so this requires no change to `memory(string)`.

---

## Specialization

`Specialise.specCall` gains a dedicated `Str.fromString` case, parallel to the
existing `Int.fromInteger` case:

- result type `string`         → identity (keep the `StrLit`, drop the call).
- result type `memory(string)` → rewrite to `memStringFromLit` (intercepted by
  EmitHull).

Concretely, mirror the `Int.fromInteger` case at `Specialise.hs:395`:

```haskell
specCall (Id (QualName (Name "Str") "fromString") ty) args _ = do
  args' <- mapM (\a -> specExp a (typeOfTcExp a)) args
  s <- getSpSubst
  let resultTy = snd (splitTy (applytv s ty))
  if resultTy == memString          -- TyCon "memory" [string]
    then pure (Id (Name "memStringFromLit") (Prim.string :-> memString), args')
    else pure (Id (QualName (Name "Str") "fromString") (Prim.string :-> resultTy), args')
```

This case must appear **before** the generic `comptimeBuiltins` guard
(`Specialise.hs:403`), exactly as the `Int` case does.

### Purity bookkeeping (correctness-critical)

The two `Str.fromString` outcomes have **opposite** purity:

- The identity path keeps `QualName (Name "Str") "fromString"`.  This name must
  be a `builtinPureFun` **and** have a MastEval identity clause, so it folds and
  is substituted away:
  ```haskell
  evalPrimitive (QualName (Name "Str") "fromString") [x] = Just x
  ```
  (Mirror the existing `Int.fromInteger` identity clause, `MastEval.hs:462`.)
  Add `QualName strClassName "fromString"` to the pure/comptime name lists (a
  new `stringPrimNames`, or extend the existing list feeding `builtinPureFuns`
  and `comptimeBuiltins`).
- `memStringFromLit` must be **excluded** from `builtinPureFuns` /
  `comptimeBuiltins` and have **no** `evalPrimitive` clause — folding it would be
  wrong (it has no comptime value; it emits code).  It is consumed only by the
  EmitHull intercept.

Register `memStringFromLit : string -> memory(string)` as a primitive scheme in
`Primitives.hs` (so it is well-formed where referenced), but keep it out of the
pure lists.  The existing `concatLit`/`strlenLit`/`keccakLit` folders are
unaffected.

---

## Comptime-only enforcement

`markIntegerComptime` was generalized to `markComptimeOnly` (`TcStmt.hs`), which
forces `comptime` on any `let` whose type is comptime-only (`integer` **or**
`string`).  Such bindings are then folded and eliminated by MastEval before hull
emission, so a `string`-typed `let` never reaches `translateMastType`.

The SAIL-level checker (`ComptimeCheck.hs`) likewise treats a **parameter or
return of comptime-only type as comptime** (`isComptimeOnlyTy` / `effRetComptime`):
a `string` value exists only at compile time, so `concatLit`/`strlenLit`/
`keccakLit` — and any user function over `string` — are implicitly comptime
without annotation.  This is the consistent fix: *explicitly* annotating
`concatLit` `comptime` would cascade (its caller `string:Add.add` passes its own
unannotated `string` params to it, which the checker would then reject); treating
all `string`/`integer` values as comptime by type avoids that, because those
caller params are implicitly comptime too.

Backstops if a `string` value somehow survives: `emitLit (StrLit _) = error`
(`EmitHull.hs:220`) and `translateMastType` of `string` (an empty `data string;`)
fails with "empty sum string".  These are crash-level guards, not the clean
per-construct messages the `integer` path has (`emitFunDef`/`translateParam`/
`emitStmt` at `mastIntegerTy`); dedicated `string` guard messages are minor
future polish (see gaps).  The `string-mem-runtime-fail` test shows the common
case (a `string` returned where `memory(string)` is expected) is caught earlier,
at type checking.

## Interaction with `comptime`

`comptime` is an **enforcement** annotation (handled by ComptimeCheck), not a
hint: it requires the binding be compile-time-evaluable and is a type error
otherwise.  A plain binding merely *allows* folding and falls back to runtime.
Verified behaviour:

| Binding | Result |
|---|---|
| `let a : word = x + 2` (runtime `x`) | ✅ compiles; runtime `addWord` |
| `let a : comptime word = x + 2` (runtime `x`) | ❌ *"comptime let 'a' is bound to a runtime expression"* |
| `let a : comptime word = 2 + 2` | ✅ folds to `4` |
| `let s : comptime string = "a" + "b"` | ✅ folds to `StrLit "ab"` |
| `let a : comptime memory(string) = "x"` | ❌ same "runtime expression" error |

Two consequences specific to strings:

- **`string` is implicitly comptime.**  `markComptimeOnly` already forces
  `comptime` on `string`-typed lets, so the annotation on
  `let s : comptime string = …` is redundant (but legal).
- **`memory(string)` is a runtime type.**  `comptime memory(string)` is rejected,
  because materialization (`mload(0x40)` + `mstore`s) is runtime code.  The
  string's *content* is comptime (concatenation folds; the bytes are baked into
  `__strlit_<n>` as constants), but *placing* it in memory is runtime.

### When does a `comptime string` materialize?

**It does not — on its own.**  `let s : comptime string = "Hello " + "world"`
binds a comptime value: `+` folds to `StrLit "Hello world"` and the comptime
`let` is eliminated by dead-let substitution.  It emits **zero** runtime code (no
`__strlit_*`, no `mstore`).  Materialization happens **only** where the string is
converted to a runtime representation — `Str.fromString(s)` at a `memory(string)`
site (which lowers to `memStringFromLit` → a `__strlit_<n>` call).  If `s`
instead flows to a comptime sink (`revertLit`, `strlenLit`, `keccakLit`, another
`concatLit`, …) it never touches memory.  Each conversion site emits a runtime
call to the content-shared allocator, so the allocation runs **per use site, at
runtime**, not at the binding.

The two axes are **orthogonal**: `comptime` controls *when* (enforced
compile-time vs opportunistic); the value's type controls *where the work
happens* (comptime `string`/`integer` domain vs runtime `memory(string)`/`word`).
`Str.fromString @ memory(string)` is precisely the point where comptime content
crosses into a runtime value — the dual of `Int.fromInteger @ word` for integers.

## Usability: implicit conversion at runtime sites (proposed)

Today a **bare** literal at a `memory(string)` site works
(`return "abcd";` → materialized), but a string built with a comptime **operation**
does not:

```solidity
let a : memory(string) = "Hello, " + "world!";   // ERROR: no instance memory(string) : Add
```

After desugaring this is `Add.add(Str.fromString("Hello, "), Str.fromString("world!"))`,
and the `memory(string)` annotation forces `Add`'s result to `memory(string)`,
which has no `Add` instance.  The user must wrap the whole expression:
`Str.fromString("Hello, " + "world!")`.  (The numeric analog `let a : word = 2 + 2`
works only because `word : Add` exists; for `+`/`-`/`*` word arithmetic is
observationally equal to integer-then-truncate, so the missing "operate at the
comptime type, convert the result" is invisible there.  Strings have no such
escape hatch — concatenation is not available at `memory(string)` at all.)

**Proposed fix — expected-type-directed coercion (subsumption).**  At an
ascription site (`let x : T = e`, `return e`, typed argument), if `e` naturally
has a comptime-only type `Tc` and the expected type `T` has a `T : Str` / `T : Int`
instance, wrap `e` in `Str.fromString` / `Int.fromInteger`.  For
`"Hello, " + "world!"`, the result variable carries exactly `{:Str, :Add}`, and
`string` is the **unique** type satisfying both — so it resolves at `string`
(concat folds) and the ascription materializes the result once.  The same
machinery gives exact-integer semantics for the numeric case.

**Why it is not a quick patch (grounded).**  The `memory(string) : Add` failure
does not surface at the `let`; the `let` returns its predicates and they are
discharged later, at the function-level `reduce` (`TcStmt.hs:639`).  So a local
"try, then coerce on failure" at the ascription is infeasible.  A real fix means
either (a) changing ascription checking from "push `T` into `e`" to
"infer `e`, then coerce", which risks regressions wherever a `let`/`return`
currently relies on the expected type to resolve a polymorphic RHS; or (b) a
defaulting rule that resolves a `{:Str, :Add}` (more generally
comptime-class + operation-class) variable to its unique satisfying type.  Both
reintroduce a form of the implicit coercion the literal-wrapping design
deliberately replaced, so this is an architectural decision to take deliberately,
not improvise.  Scope is bounded (only annotations whose type has a `Str`/`Int`
instance — today just `memory(string)` / `word` / `integer`), so a guarded,
test-gated implementation is plausible as a follow-up.

---

## Ordered implementation steps

Each step leaves all existing tests passing.

1. **`Primitives.hs` / `TcEnv.hs` / `NameResolution.hs`** — add `strClassName =
   "Str"`, `fromStringScheme : forall a. (a:Str) => string -> a`, the
   `fromString` entry, the two primitive instances (`string`, `memory(string)`),
   and the `memStringFromLit` primitive name.  Reserve the class name.  Follow
   the `Int` additions (noting the compound `memory(string)` instance head).

2. **`StrLiteralDesugar.hs`** + `sol-core.cabal` module entry + pipeline wiring
   in `SolcorePipeline.hs` (clone `IntLiteralDesugar`).  `tcLit`/`typeOfTcExp`
   for `StrLit` already return `string`.

3. **`TcSimplify.hs`** — skip `Str` constraints in the `noDesugarCalls` validity
   checks, as for `Int`.

4. **`Specialise.hs`** — `specCall` case resolving `Str.fromString` to identity
   (`string`) or `memStringFromLit` (`memory(string)`).

5. **`EmitHull.hs`** — intercept `memStringFromLit [StrLit s]`: compute layout,
   register the self-contained per-literal `__strlit_<n>` function, rewrite
   the call, prepend the generated functions to the object.  Comptime-only
   enforcement is handled by generalizing `markIntegerComptime` to
   `markComptimeOnly` in `TcStmt.hs` (covers `integer` and `string`).

6. **Tests** — see below.

7. **This doc** — update with results, gaps, and the `code(string)` follow-up if
   pursued.

---

## Tests

Source-level (`test/examples/comptime/`, registered in `test/Cases.hs`).  These
compile through `emitHull` (the test path stops at Hull objects; runtime/ABI
value checks would need a `dispatch` testrunner case — see gaps).  The canonical
style is **A1** (apply `Str.fromString` to the comptime expression directly);
`string-concat-mem.solc` additionally exercises **A2** (`let s : string = …`
then `Str.fromString(s)`), the `string`-typed-`let` + dead-let-substitution
path.

- **`string-lit-mem.solc`** (A1) — `main()` returns `"abcd"` as `memory(string)`.
  The simplest materialization; the generated `__strlit_0` writes length 4 and
  the packed word.  Verified through `yule` (Hull→Yul typechecks).
- **`string-concat-mem.solc`** — `main()` computes `strlen` of `"Hello, " +
  "world!"` materialized two ways: A1 (`Str.fromString("Hello, " + "world!")`)
  and A2 (a `greetingLet` helper using a `string` `let`).  `+` folds via
  `concatLit` in the `string` domain, so both forms produce the same `StrLit`
  and share **one** `__strlit_*` allocator (len 13) — proving the
  comptime→runtime boundary and dedup across forms.
- **`string-lit-dedup.solc`** — `main()` materializes `"alpha"`, `"beta"`,
  `"alpha"`; exactly **two** `__strlit_*` allocators are emitted (the repeated
  `"alpha"` shares one).
- **`string-mem-runtime-fail.solc`** (`runTestExpectingFailure`) — returns a
  `string`-typed `let` where `memory(string)` is expected, with no conversion.
  Rejected at type checking ("memory(string) and string do not unify"),
  validating the comptime-only invariant.

Note: every test needs a `main` entry point.  With `--no-gen-dispatch` and no
`main`, the specializer has no roots and prunes all functions, so no
materialization happens — an early version of the concat/dedup tests compiled
vacuously for this reason.

Already-passing string tests unaffected by the desugaring: `string-lit-ops`,
`string-lit-len`, `string-lit-keccak` (comptime `concatLit`/`strlenLit`/
`keccakLit` folding) and `string-const`.

---

## Known gaps and future work

| Gap | Impact | Resolution |
|---|---|---|
| Inline `mstore` materializer code size | Large literals bloat bytecode | `code(string) : Str` data-section path |
| `code(a)` type constructor | No data-section strategy yet | Future additive instance + `codecopy` conversion |
| No `string`-taking helper (`toMemory`) | Must apply `Str.fromString` where the literal is in scope | "Always inline" annotation for helpers (own work) |
| Source `Str` instance cannot materialize | `instance Error : Str` typechecks and specialises but hits the comptime-only guard at emission, so `require(cond, "msg")` still needs `requireStr` | Literal-keyed specialisation: extend the `Resolution` key with comptime-only-typed argument values and substitute them into the specialised body |
| String literals in constructor/deploy code | Allocators emitted into runtime object only; a deploy-side call would not resolve | Emit allocators into the object that references them, or duplicate |
| ~~No runtime/ABI value test~~ | ~~correctness of returned bytes unchecked~~ | ✓ `dispatch/stringlit.json` returns `memory(string)`, asserted via evmone |
| ~~No clean `string` EmitHull guard message~~ | ~~"empty sum string" crash~~ | ✓ `comptimeOnlyMastName` guards on return/param/let (`EmitHull.hs`) |
| `string` binding without `comptime` | Forced comptime by `markComptimeOnly`; if it still survived, clean guard message now | Enforce `comptime` in SAIL/MAST checkers (same gap as `integer`) |
| `let s = "x"` (unannotated) | Result representation ambiguous (`string` vs `memory(string)`) | Default rule (e.g. to `memory(string)`) — mirror the integer `word` default question |
| String literal patterns | Runtime string-match unsupported; comptime-label match not planned | Separate feature if needed |
| UTF-8 vs byte semantics | `len` is UTF-8 byte length (matches `strlenLit`) | Documented; consistent with Solidity `bytes`/`string` |
