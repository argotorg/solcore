module Solcore.Backend.MastEval
  ( evalCompUnit
  ) where

{- Partial Evaluator for Mast
   Performs compile-time evaluation where possible:
   - Folds calls to `addWord` with literal arguments
   - Propagates known variable values
   - Inlines simple pure functions with literal arguments
-}

import qualified Data.Map.Strict as Map
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt(Literal(..))

-----------------------------------------------------------------------
-- Data structures
-----------------------------------------------------------------------

-- Variable environment: variable id (name + type) -> known value
-- Uses full MastId to distinguish variables with same name but different types
type VEnv = Map.Map MastId MastExp

-- Function table: function name -> definition
type FunTable = Map.Map Name MastFunDef

-- Fuel for controlling recursion depth during inlining
type Fuel = Int

defaultFuel :: Fuel
defaultFuel = 10

-----------------------------------------------------------------------
-- Main entry point
-----------------------------------------------------------------------

evalCompUnit :: MastCompUnit -> MastCompUnit
evalCompUnit cu = cu { mastTopDecls = map (evalTopDecl funTable) (mastTopDecls cu) }
  where
    funTable = buildFunTable cu

-----------------------------------------------------------------------
-- Build function table from compilation unit
-----------------------------------------------------------------------

buildFunTable :: MastCompUnit -> FunTable
buildFunTable cu = Map.fromList $ concatMap collectFromTopDecl (mastTopDecls cu)
  where
    collectFromTopDecl :: MastTopDecl -> [(Name, MastFunDef)]
    collectFromTopDecl (MastTContr c) = collectFromContract c
    collectFromTopDecl (MastTDataDef _) = []

    collectFromContract :: MastContract -> [(Name, MastFunDef)]
    collectFromContract c = concatMap collectFromDecl (mastContrDecls c)

    collectFromDecl :: MastContractDecl -> [(Name, MastFunDef)]
    collectFromDecl (MastCFunDecl fd) = [(mastFunName fd, fd)]
    collectFromDecl (MastCMutualDecl ds) = concatMap collectFromDecl ds
    collectFromDecl (MastCDataDecl _) = []

-----------------------------------------------------------------------
-- Evaluate top-level declarations
-----------------------------------------------------------------------

evalTopDecl :: FunTable -> MastTopDecl -> MastTopDecl
evalTopDecl ft (MastTContr c) = MastTContr (evalContract ft c)
evalTopDecl _ d@(MastTDataDef _) = d

evalContract :: FunTable -> MastContract -> MastContract
evalContract ft c = c { mastContrDecls = map (evalContractDecl ft) (mastContrDecls c) }

evalContractDecl :: FunTable -> MastContractDecl -> MastContractDecl
evalContractDecl ft (MastCFunDecl fd) = MastCFunDecl (evalFunDef ft fd)
evalContractDecl ft (MastCMutualDecl ds) = MastCMutualDecl (map (evalContractDecl ft) ds)
evalContractDecl _ d@(MastCDataDecl _) = d

-----------------------------------------------------------------------
-- Evaluate function definitions
-----------------------------------------------------------------------

evalFunDef :: FunTable -> MastFunDef -> MastFunDef
evalFunDef ft fd = fd { mastFunBody = body' }
  where
    (_, body') = evalStmts ft Map.empty (mastFunBody fd)

-----------------------------------------------------------------------
-- Evaluate statements
-----------------------------------------------------------------------

-- Process statements left-to-right, threading environment through
evalStmts :: FunTable -> VEnv -> [MastStmt] -> (VEnv, [MastStmt])
evalStmts ft = go
  where
    go env [] = (env, [])
    go env (s:ss) =
      let (env', s') = evalStmt ft env s
          (env'', ss') = go env' ss
      in (env'', s' : ss')

evalStmt :: FunTable -> VEnv -> MastStmt -> (VEnv, MastStmt)
evalStmt ft env stmt = case stmt of
  MastLet i ty mInit ->
    let mInit' = fmap (evalExp ft defaultFuel env) mInit
        env' = case mInit' of
          Just e | isLiteral e -> Map.insert i e env
          _ -> Map.delete i env  -- Shadow/remove any existing binding
    in (env', MastLet i ty mInit')

  MastAssign i e ->
    let e' = evalExp ft defaultFuel env e
        env' = if isLiteral e'
               then Map.insert i e' env
               else Map.delete i env  -- Value no longer known
    in (env', MastAssign i e')

  MastStmtExp e ->
    (env, MastStmtExp (evalExp ft defaultFuel env e))

  MastReturn e ->
    (env, MastReturn (evalExp ft defaultFuel env e))

  MastMatch e alts ->
    let e' = evalExp ft defaultFuel env e
        alts' = map (evalAlt ft env) alts
    in (env, MastMatch e' alts')

  MastAsm yul ->
    -- Assembly blocks are opaque; we don't know what they modify
    -- Conservative: clear all variable bindings
    (Map.empty, MastAsm yul)

evalAlt :: FunTable -> VEnv -> MastAlt -> MastAlt
evalAlt ft env (pat, body) =
  -- Pattern bindings shadow existing bindings, but we don't track them
  -- (conservative: treat all pattern-bound vars as unknown)
  let (_, body') = evalStmts ft env body
  in (pat, body')

-----------------------------------------------------------------------
-- Evaluate expressions
-----------------------------------------------------------------------

evalExp :: FunTable -> Fuel -> VEnv -> MastExp -> MastExp
evalExp ft fuel env expr = case expr of
  MastLit _ -> expr

  MastVar i ->
    case Map.lookup i env of
      Just lit -> lit
      Nothing -> expr

  MastCall i args ->
    let args' = map (evalExp ft fuel env) args
        fname = mastIdName i
    in case evalPrimitive fname args' of
         Just result -> result
         Nothing ->
           -- Try inlining if we have fuel
           -- Works for: (1) all args known, or (2) constant functions that ignore args
           if fuel > 0
           then case tryInline ft (fuel - 1) fname args' of
                  Just result -> result
                  Nothing -> MastCall i args'
           else MastCall i args'

  MastCon i es ->
    MastCon i (map (evalExp ft fuel env) es)

  MastCond e1 e2 e3 ->
    -- Evaluate all branches (conservative approach)
    -- Could potentially simplify if condition is known literal
    let e1' = evalExp ft fuel env e1
        e2' = evalExp ft fuel env e2
        e3' = evalExp ft fuel env e3
    in MastCond e1' e2' e3'

-----------------------------------------------------------------------
-- Primitive evaluation
-----------------------------------------------------------------------

evalPrimitive :: Name -> [MastExp] -> Maybe MastExp
evalPrimitive (Name "addWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a + b)))
evalPrimitive _ _ = Nothing

-----------------------------------------------------------------------
-- Function inlining
-----------------------------------------------------------------------

-- Try to inline a function call.
-- Works when: (1) all arguments are known values, or
--             (2) function is "constant" (ignores its arguments)
tryInline :: FunTable -> Fuel -> Name -> [MastExp] -> Maybe MastExp
tryInline ft fuel fname args = do
  fd <- Map.lookup fname ft
  -- Check that function is inlinable (no assembly blocks)
  if not (isInlinable fd)
    then Nothing
    else do
      -- Bind parameters to argument values
      let params = mastFunParams fd
      if length params /= length args
        then Nothing
        else do
          let paramToId p = MastId (mastParamName p) (mastParamType p)
          let env = Map.fromList $ zip (map paramToId params) args
          -- Evaluate the function body and extract return value
          evalFunBody ft fuel env (mastFunBody fd)

-- Check if a function can be inlined
isInlinable :: MastFunDef -> Bool
isInlinable fd = all isInlinableStmt (mastFunBody fd)
  where
    isInlinableStmt (MastAsm _) = False
    isInlinableStmt (MastLet _ _ _) = True
    isInlinableStmt (MastAssign _ _) = True
    isInlinableStmt (MastStmtExp _) = True
    isInlinableStmt (MastReturn _) = True
    isInlinableStmt (MastMatch _ alts) = all (all isInlinableStmt . snd) alts

-- Evaluate a function body and extract the return value
evalFunBody :: FunTable -> Fuel -> VEnv -> [MastStmt] -> Maybe MastExp
evalFunBody ft fuel = go
  where
    go _ [] = Nothing  -- No return statement found
    go env (stmt:rest) = case stmt of
      MastLet i _ mInit ->
        let mInit' = fmap (evalExp ft fuel env) mInit
            env' = case mInit' of
              Just e | isLiteral e -> Map.insert i e env
              _ -> Map.delete i env  -- Shadow/remove any existing binding
        in go env' rest

      MastAssign i e ->
        let e' = evalExp ft fuel env e
            env' = if isLiteral e'
                   then Map.insert i e' env
                   else Map.delete i env
        in go env' rest

      MastStmtExp _ -> go env rest

      MastReturn e ->
        let e' = evalExp ft fuel env e
        in if isLiteral e' then Just e' else Nothing

      MastMatch _ _ -> Nothing  -- Don't inline functions with match statements

      MastAsm _ -> Nothing  -- Should not happen due to isInlinable check

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- Check if an expression is a "known value" suitable for inlining/propagation.
-- This includes literals and constructors with all-known arguments.
isKnownValue :: MastExp -> Bool
isKnownValue (MastLit _) = True
isKnownValue (MastCon _ args) = all isKnownValue args
isKnownValue _ = False

-- Backward compatibility alias
isLiteral :: MastExp -> Bool
isLiteral = isKnownValue
