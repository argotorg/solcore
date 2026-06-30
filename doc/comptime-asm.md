# Comptime Yul Interpreter: Design and Extension Plan

## Status

The initial Yul arithmetic interpreter is implemented in `src/Solcore/Backend/MastEval.hs`.
It supports `add` and `mul`, replacing the earlier hardcoded `addWord`/`mulWord` builtins.

---

## What is implemented

### YulState

```haskell
type YulState = Map.Map Name Integer  -- variable bindings only (no memory yet)
```

### Supported operations

`evalYulOp` handles: `add`, `mul` (256-bit, wrapping mod 2^256).

`evalYulStmt` handles: `YAssign [n] e` — single-target assignment.

`evalYulBlock` evaluates a sequence of statements threading `YulState`.

### Purity classification

`asmIsInterpretable :: [YulStmt] -> Bool` is used by `stmtIsPure` to decide whether
a function whose body contains an asm block belongs in `pureFuns`.  This is load-bearing
for comptime-check soundness: a function containing `sload` must **not** enter `pureFuns`,
or `isComptime` in `Backend/ComptimeCheck.hs` would wrongly classify its result as comptime.

The rule: `stmtIsPure _ (MastAsm stmts) = asmIsInterpretable stmts`.

Optimistic treatment (`True` for all asm) breaks negative tests because it lets
`sloadWord` into `pureFuns`.

### TypeReg

```haskell
type TypeReg = Map.Map Name MastId
```

Pre-scanned from the whole function body (params + all `let` declarations).
Needed because `let rw : word;` (no initialiser) deletes `rw` from `VEnv` before
the asm block that assigns it, so without `TypeReg` we cannot reconstruct the
`MastId` to merge the Yul-computed value back.

### Inlining flow

`addWord`/`mulWord` are no longer special-cased.  They enter `pureFuns` via
`asmIsInterpretable`, and `tryInline` can inline them.  `evalFunBody` processes
the body:

```
MastLet False rw_id word Nothing   → delete rw from VEnv
MastAsm [YAssign ["rw"] (add l r)] → evalYulBlock succeeds, mergeYulStateToVEnv
                                      uses TypeReg to find rw_id, inserts rw=result
MastReturn (MastVar rw_id)         → evalExp finds rw in VEnv, returns literal
```

---

## Memory extension plan

### Byte-level memory representation

EVM memory is a flat, infinite, zero-initialised byte array.  The three relevant ops:

| Op | EVM semantics |
|---|---|
| `mstore(p, v)` | `mem[p..p+31] := big_endian(v)` (256-bit) |
| `mload(p)` | `← mem[p..p+31]` as 256-bit big-endian word |
| `mstore8(p, v)` | `mem[p] := v & 0xff` (one byte) |

Use a byte-level sparse map: `Map.Map Integer Word8`.

`mload` returns `Nothing` if any of the 32 bytes at `p..p+31` are not in the map.
This is the correct semantics for comptime evaluation: we can only return a value
if all 32 bytes were written in the current evaluation context.  We cannot assume
unwritten bytes are 0, because runtime code may have written to memory before the
function we are evaluating runs (e.g. the free memory pointer at slot 64 is written
by EVM initialization code; treating it as 0 would cause the PE to fold
`get_free_memory()` to 0 at compile time, corrupting the ABI encoder).

This handles overlapping stores correctly: two `mstore`s at different addresses that
cover all 32 bytes of a `mload` range produce a well-defined result — it is NOT
correct to return `Nothing` if all bytes are covered.

#### Pure memory helpers

These are pure functions, not in any monad.  They require `Data.Bits` (`shiftR`, `.&.`)
and `Data.Word` (`Word8`), both of which need to be imported in `MastEval.hs`
(`Word8` is already imported):

```haskell
-- write 32 bytes big-endian
mstoreBytes :: Integer -> Integer -> Map Integer Word8 -> Map Integer Word8
mstoreBytes p v mem =
  foldl' (\m i -> Map.insert (p + i)
                              (fromIntegral ((v `shiftR` (8 * (31 - fromIntegral i))) .&. 0xff))
                              m)
         mem [0..31]

-- read 32 bytes; Nothing if any byte is missing from the map
mloadWord :: Integer -> Map Integer Word8 -> Maybe Integer
mloadWord p mem =
  foldl'
    (\mAcc i -> do acc <- mAcc; b <- Map.lookup (p + i) mem; pure (acc * 256 + fromIntegral b))
    (Just 0)
    [0..31]
```

`mstore8(p, v)` is `Map.insert p (fromIntegral (v .&. 0xff)) mem`.

### YulState stays unchanged

`YulState` remains local Yul variable bindings, threaded explicitly as before.
Memory lives in `EvalM`'s state (see next section).

```haskell
type YulState = Map.Map Name Integer  -- (unchanged)
```

### EvalM state: add memory alongside fuel

Replace `EvalM`'s state type.  Currently `State Fuel`; becomes `State EvalState`:

```haskell
data EvalState = EvalState
  { esFuel :: !Fuel
  , esMem  :: !(Map.Map Integer Word8)
  }

type EvalM = ReaderT EvalEnv (State EvalState)
```

`runEvalM` constructs `EvalState` with empty memory and extracts fuel from the result:

```haskell
runEvalM :: EvalEnv -> Fuel -> EvalM a -> (a, Fuel)
runEvalM env fuel m =
  let initState = EvalState { esFuel = fuel, esMem = Map.empty }
      (a, finalState) = runState (runReaderT m env) initState
  in (a, esFuel finalState)
```

Fuel accessors update to use the `esFuel` field:

```haskell
getFuel :: EvalM Fuel
getFuel = lift $ gets esFuel

useFuel :: EvalM Bool
useFuel = do
  f <- getFuel
  if f > 0
    then do lift $ modify (\s -> s { esFuel = esFuel s - 1 }); pure True
    else pure False

restoreFuel :: EvalM ()
restoreFuel = lift $ modify (\s -> s { esFuel = esFuel s + 1 })
```

Two new helpers for memory access within `EvalM`:

```haskell
getsMem :: EvalM (Map.Map Integer Word8)
getsMem = lift $ gets esMem

modifyMem :: (Map.Map Integer Word8 -> Map.Map Integer Word8) -> EvalM ()
modifyMem f = lift $ modify (\s -> s { esMem = f (esMem s) })
```

### Yul evaluator: lift into EvalM

The Yul evaluator functions move from pure `Maybe` to `EvalM (Maybe ...)` so they
can access `esMem`.  `YulState` is still threaded explicitly — no new monad layer.

New signatures (replacing the current pure ones):

```haskell
evalYulExp   :: YulState -> YulExp    -> EvalM (Maybe Integer)
evalYulOp    :: Name -> [Integer]     -> EvalM (Maybe Integer)
evalYulStmt  :: YulState -> YulStmt   -> EvalM (Maybe YulState)
evalYulBlock :: YulState -> [YulStmt] -> EvalM (Maybe YulState)
```

For `add`/`mul`, the bodies just wrap the existing pure logic in `pure`.
The `EvalM` is only exercised by `mload` (reads `esMem`) and `mstore`/`mstore8`
(writes `esMem`).

Callers in `evalStmt` and `evalFunBody` already run in `EvalM`, so the change
at call sites is mechanical: replace `case evalYulBlock ... of` with bind-then-case.

The *bodies* of `evalYulBlock` and `evalYulExp` need more care, because they
currently rely on `Maybe`'s bind for short-circuiting.  After the lift, the
`Maybe` is inside `EvalM`, so short-circuiting must be explicit:

```haskell
-- evalYulBlock: currently uses Maybe's bind to short-circuit on Nothing.
-- After lift, needs explicit case:
evalYulBlock env (s : ss) = do
  menv' <- evalYulStmt env s
  case menv' of
    Nothing   -> pure Nothing
    Just env' -> evalYulBlock env' ss

-- evalYulExp general YCall case: currently uses Maybe's mapM to collect args.
-- After lift, mapM is EvalM's (giving [Maybe Integer]), so use sequence:
evalYulExp env (YCall op args) = do
  mvals <- mapM (evalYulExp env) args
  case sequence mvals of
    Nothing   -> pure Nothing
    Just vals -> evalYulOp op vals
```

### evalYulStmt: new cases for mstore and mstore8

`mstore`/`mstore8` appear as `YExp` (expression-statement) nodes in the Yul AST.
Add two new patterns before the catch-all:

```haskell
evalYulStmt env (YExp (YCall (Name "mstore") [pExp, vExp])) = do
  mp <- evalYulExp env pExp
  mv <- evalYulExp env vExp
  case (mp, mv) of
    (Just p, Just v) -> do
      modifyMem (mstoreBytes p v)
      pure (Just env)       -- YulState unchanged; memory updated via EvalM
    _ -> pure Nothing

evalYulStmt env (YExp (YCall (Name "mstore8") [pExp, vExp])) = do
  mp <- evalYulExp env pExp
  mv <- evalYulExp env vExp
  case (mp, mv) of
    (Just p, Just v) -> do
      modifyMem (Map.insert p (fromIntegral (v .&. 0xff)))
      pure (Just env)
    _ -> pure Nothing
```

#### Partial memory mutation on block failure

If a block succeeds on an `mstore` but fails on a later statement,
`esMem` retains the write even though `evalYulBlock` returns `Nothing`.
This is harmless for two reasons:

1. **Inlining path** (`evalFunBody`): a function containing an uninterpretable
   op (e.g. `sload`) never enters `pureFuns` (blocked by `asmIsInterpretable`),
   so `evalYulBlock` is never attempted on such a block during inlining.

2. **AST-transform path** (`evalStmt`): on `Nothing`, `VEnv` is cleared.
   The stale `esMem` entries are scoped to the current `evalFunDef` and
   cannot affect correctness — variable state was already discarded, and
   memory is reset at the next `evalFunDef`.

If this becomes a concern, the simplest fix is snapshotting `esMem` before
`evalYulBlock` and restoring on `Nothing`.

### evalYulExp: mload case and clause ordering

`mload` reads 32 bytes from `esMem`.  It succeeds only when the address is known
AND all 32 bytes at that address have been written in the current evaluation context:

```haskell
evalYulExp env (YCall (Name "mload") [pExp]) = do
  mp <- evalYulExp env pExp
  case mp of
    Just p -> do
      mem <- getsMem
      pure (mloadWord p mem)  -- Nothing if any byte missing from esMem
    Nothing -> pure Nothing
```

The full clause order in `evalYulExp` after the change (order matters — `mload`
must precede the general `YCall` catch-all):

1. `YIdent n` → variable lookup in `YulState`
2. `YLit (YulNumber n)` → literal
3. `YLit YulTrue` / `YLit YulFalse` → 1 / 0
4. `YCall (Name "mload") [pExp]` → memory read via `esMem` (**new**)
5. `YCall op args` → general: evaluate args, delegate to `evalYulOp`
6. `_ → pure Nothing`

### Memory scope

**Per `evalFunDef` invocation, shared across all inlined calls within it.**

Rationale:
- `evalFunDef` is not reentrant: it is called once per top-level function from
  `evalContractDecl` and never from within `evalStmt`/`evalFunBody`/`tryInline`.
- The recursive inlining chain runs entirely through
  `evalFunBody → evalExp → tryInline → evalFunBody`.
- All of these run inside the same `EvalM` execution, so they share `esMem`.
- Functions subject to comptime evaluation are always inlined — they never become
  separate on-chain calls.  Internal calls share EVM memory; resetting it at inline
  boundaries would be wrong.

Reset `esMem` at the start of each `evalFunDef`:

```haskell
evalFunDef :: MastFunDef -> EvalM MastFunDef
evalFunDef fd = do
  modifyMem (const Map.empty)
  let tyReg = buildTypeReg (mastFunParams fd) (mastFunBody fd)
  (_, body') <- evalStmts tyReg Map.empty (mastFunBody fd)
  pure $ fd { mastFunBody = body' }
```

Each top-level function starts with clean memory.  Memory does NOT persist
across separate `evalFunDef` calls, which matches EVM semantics where each
call frame starts with fresh memory.

Example of correct memory propagation across inlined calls within one function:

```solcore
function store(a : word, v : word) { assembly { mstore(a, v) } }
function load(a : word) -> word    { let r : word; assembly { r := mload(a) } return r; }

// at the call site, both inlined into the same evalFunDef run:
store(0, 42);          // esMem: mem[0..31] = big_endian(42)
let x = load(0);      // esMem lookup at 0 → 42; x evaluates to 42
```

Without shared memory across the inline boundary, `load(0)` would see empty memory
and return 0 (not the stored 42), breaking the comptime evaluation.

### asmIsInterpretable extension

Extend to accept `mstore`/`mload`/`mstore8`:

```haskell
interpretableStmt (YAssign [_] e)                          = interpretableExp e
interpretableStmt (YExp (YCall (Name "mstore")  [p, v]))   = interpretableExp p && interpretableExp v
interpretableStmt (YExp (YCall (Name "mstore8") [p, v]))   = interpretableExp p && interpretableExp v
interpretableStmt _ = False

interpretableExp (YCall (Name "mload") [p])  = interpretableExp p
interpretableExp (YCall op args) =
  interpretableOp op (length args) && all interpretableExp args

interpretableOp (Name "add") 2 = True
interpretableOp (Name "mul") 2 = True
interpretableOp _ _            = False
```

Note: `mload` is an interpretable *expression* (returns a value), while
`mstore`/`mstore8` are interpretable *statements* (side-effect only).
`mload` has its own clause in `interpretableExp` rather than going through
`interpretableOp`, because it accesses memory.

### Memory ops are gated on `envComptimeMode`

`asmIsInterpretable` classifies `mstore`/`mstore8`/`mload` as interpretable (so
functions using them remain in `pureFuns`), but the evaluator only *executes* them
when `envComptimeMode = True`:

- **`mload`** in `evalYulExp`: returns `Nothing` if `envComptimeMode = False`.
- **`mstore`/`mstore8`** in `evalYulStmt`: return `Nothing` (abort the block) if
  `envComptimeMode = False`.

`envComptimeMode` is set to `True` by `withComptimeMode`, which is called when
evaluating the initialiser of a `let x : comptime = ...` binding in `evalStmt`.
It propagates automatically to all inlined calls because it lives in the `Reader`
layer of `EvalM`.

**Why mstore failure aborts the block (not a no-op):**
A no-op would let the block continue and later computations (e.g., `keccak256`) might
read from empty `esMem`, producing silently wrong results.  Aborting immediately means
any function whose body writes memory is not inlined outside comptime context — safe
and conservative.

**Effect on allocator functions:**
`get_free_memory` / `set_free_memory` / `allocate_memory` stay in `pureFuns`
(their asm is interpretable) but cannot be inlined in normal PE: the mload/mstore in
their bodies immediately returns `Nothing`, so `evalFunBody` propagates `Nothing`,
and the call site is left unevaluated.  In a comptime chain, if `get_free_memory` is
called before anything has written to address 64 in `esMem`, `mloadWord` returns
`Nothing` → inlining fails → the `let x : comptime` binding is not folded.  Correct.

### Future: keccak256 over known memory

The pattern in `hash1`/`hash2` in `std/NumLib.solc`:

```
mstore(0, x)
result := keccak256(0, 32)
```

When `x` is a known comptime value, the 32 bytes at `mem[0..31]` are known, and
`keccak256` can be evaluated at compile time (the infrastructure already exists
in `evalPrimitive` for `keccakLit`).  This requires reading a contiguous byte range
from `esMem` — straightforward with the byte-level representation.

---

## Implementation order

Each step should leave all existing tests passing.

### Step 1: EvalM state change + lift Yul evaluator into EvalM

Two logically distinct changes, done together to avoid an intermediate state
where `getsMem`/`modifyMem` exist but have no callers:

**EvalM state**:
- Add `data EvalState` with `esFuel` and `esMem` fields
- Change `type EvalM = ReaderT EvalEnv (State EvalState)`
- Update `runEvalM`, `getFuel`, `useFuel`, `restoreFuel`
- Add `getsMem`, `modifyMem` helpers
- Add `modifyMem (const Map.empty)` at top of `evalFunDef`
- Add `import Data.Bits (shiftR, (.&.))` (needed in later steps but harmless now)

**Yul evaluator lift**:
- Change signatures of `evalYulExp`, `evalYulOp`, `evalYulStmt`, `evalYulBlock`
  from pure `Maybe` to `EvalM (Maybe ...)`.  Bodies wrap existing logic with `pure`.
- Update callers in `evalStmt`/`evalFunBody` to use monadic bind.

**Exports**: the unit tests need to run `EvalM` actions.  Add to the module's
export list: `runEvalM`, `EvalEnv(..)`, `EvalState(..)`.
(Or export a dedicated `runYulPure` test wrapper instead of the raw internals.)

**Test adaptation**: existing unit tests call these functions as pure.
Add a test helper:

```haskell
runPure :: EvalM a -> a
runPure m = fst $ runEvalM (EvalEnv Map.empty Set.empty) defaultFuel m
```

Update all existing test assertions, e.g.:
`runPure (evalYulOp (Name "add") [3, 5]) @?= Just 8`

No behavioral change.  All tests pass with identical results.

### Step 2: Memory helpers and their tests

Add `mstoreBytes`, `mloadWord` as pure functions.  Export them for direct
unit testing.  Unit tests:

- Store then load at same address → `Just` original value
- Load from unwritten address → `Nothing`
- Overlapping stores covering all 32 bytes: `mloadWord` returns `Just` the value
- Partial write (mstore8 only, 1 byte): `mloadWord` → `Nothing` (31 bytes missing)
- Round-trip: `mstoreBytes p v` then `mloadWord p` → `Just v`

### Step 3–5: mstore, mload, mstore8 in Yul evaluator

Add the new `evalYulStmt`/`evalYulExp` cases as described above.
Tests for each:

- `mstore(0, 42)` then `r := mload(0)` → `r == 42`
- `mstore8(0, 0xff)` then `r := mload(0)` → top byte `0xff`, rest zero
- `mstore`/`mload` with unknown operand → `Nothing`
- Chain across multiple statements in one block

### Step 6: Extend asmIsInterpretable

Add patterns as described above.  Tests:

- `mstore` expression-stmt → `True`
- `mload` in assignment → `True`
- `mstore8` expression-stmt → `True`
- `sload` still → `False`

### Step 7: Integration test

Add `.solc` test in `test/examples/comptime/` with a function wrapping
`mstore`/`mload` that should be comptime-evaluated.

### Step 8 (later): keccak256 over known memory ranges

---

## Future: ExceptT for comptime evaluation diagnostics

The current design uses `Maybe` for failure: `Nothing` means "could not evaluate
at compile time" with no further detail.  A future improvement would be to add
`ExceptT` (or replace `Maybe` with `Either`) to carry structured error information:

```haskell
data ComptimeFailure
  = UnsupportedOp Name
  | UnknownVariable Name
  | UnknownAddress
  | FuelExhausted
  | ...
```

This would improve error messages when a `comptime` annotation fails (currently
the user just sees that evaluation didn't produce a value, with no indication of
*which* operation was the obstacle).

This is independent of the memory extension and can be done separately.
