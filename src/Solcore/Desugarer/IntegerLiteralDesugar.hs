module Solcore.Desugarer.IntegerLiteralDesugar (desugarIntegerLiterals, desugarIntegerLiteralsTopDecls) where

{- Early desugaring pass: insert `wordToInteger` coercions at integer literal
   sites where the expected type is `integer`.

   Runs on the untyped AST (CompUnit Name), before type inference.

   Two-stage design:
     1. Signature collection — build a table (Name -> [param types]) from:
          - the seven integer compiler builtins (hardcoded)
          - all user-defined and class/instance functions in the CompUnit,
            stored under both unqualified and qualified names
     2. Contextual transform — walk the AST maintaining the expected type at
        each expression position (from let annotation, function return type,
        or declared parameter type from the signature table).  At each integer
        literal with expected type `integer`, insert `wordToInteger(n)`.

   Conservative rules (only coerce when certain):
     - Only literals are coerced; non-literal expressions are left unchanged.
     - Only when the expected type is exactly `TyCon "integer" []`.
     - Polymorphic params (TyVar) and unknown functions are left unchanged.
     - Unannotated lets (no explicit type) are left unchanged.
     - Already-wrapped calls like `wordToInteger(42)` are not double-wrapped
       because `wordToInteger`'s param type is `word`, not `integer`.
-}

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty (Ty (..))
import Solcore.Primitives.Primitives (word)

-----------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------

integerTy :: Ty
integerTy = TyCon (Name "integer") []

-- | Map from function name to its declared parameter types.
-- Both unqualified (Name "f") and qualified (QualName "C" "f") entries
-- are stored so that class-method calls like `Num.fromInteger(42)` are found.
type SigTable = Map.Map Name [Ty]

-----------------------------------------------------------------------
-- Hardcoded signatures for the seven integer compiler builtins
-- These are registered in primCtx and have no source Signature.
-----------------------------------------------------------------------

integerBuiltinSigs :: SigTable
integerBuiltinSigs =
  Map.fromList
    [ (Name "wordToInteger", [word]),
      (Name "wordFromInteger", [integerTy]),
      (Name "integerAdd", [integerTy, integerTy]),
      (Name "integerSub", [integerTy, integerTy]),
      (Name "integerMul", [integerTy, integerTy]),
      (Name "integerLt", [integerTy, integerTy]),
      (Name "integerEq", [integerTy, integerTy])
    ]

-----------------------------------------------------------------------
-- Stage 1: build signature table from source
-----------------------------------------------------------------------

buildSigTable :: CompUnit Name -> SigTable
buildSigTable cu =
  -- builtins take priority: they must not be shadowed by user code
  Map.union integerBuiltinSigs (Map.unions (map fromTopDecl (contracts cu)))
  where
    fromTopDecl (TFunDef fd) = fromSig Nothing (funSignature fd)
    fromTopDecl (TContr c) = Map.unions (map fromContrDecl (decls c))
    fromTopDecl (TClassDef cl) =
      Map.unions (map (fromSig (Just (className cl))) (signatures cl))
    fromTopDecl (TInstDef inst) =
      Map.unions (map (fromSig (Just (instName inst)) . funSignature) (instFunctions inst))
    fromTopDecl (TMutualDef ds) = Map.unions (map fromTopDecl ds)
    fromTopDecl _ = Map.empty -- TDataDef, TSym, TExportDecl, TPragmaDecl: no sigs

    fromContrDecl (CFunDecl fd) = fromSig Nothing (funSignature fd)
    fromContrDecl (CMutualDecl ds) = Map.unions (map fromContrDecl ds)
    fromContrDecl _ = Map.empty

    -- Store under both the unqualified name and, if a qualifier is given,
    -- the qualified name QualName qualifier methodName.
    fromSig mQualifier sig =
      let pts = map paramTy (sigParams sig)
          unqual = Map.singleton (sigName sig) pts
          qual = case mQualifier of
            Just q -> Map.singleton (QualName q (show (sigName sig))) pts
            Nothing -> Map.empty
       in Map.union unqual qual

    paramTy (Typed _ _ ty) = ty
    paramTy (Untyped _ _) = word -- no annotation: default to word, no coercion

-----------------------------------------------------------------------
-- Stage 2: contextual AST transform
-----------------------------------------------------------------------

desugarIntegerLiterals :: CompUnit Name -> CompUnit Name
desugarIntegerLiterals cu =
  cu {contracts = map (transformTopDecl sigTable) (contracts cu)}
  where
    sigTable = buildSigTable cu

desugarIntegerLiteralsTopDecls :: [TopDecl Name] -> [TopDecl Name]
desugarIntegerLiteralsTopDecls topDecls =
  contracts (desugarIntegerLiterals (CompUnit [] topDecls))

transformTopDecl :: SigTable -> TopDecl Name -> TopDecl Name
transformTopDecl st (TFunDef fd) = TFunDef (transformFunDef st fd)
transformTopDecl st (TContr c) = TContr (transformContract st c)
transformTopDecl st (TInstDef inst) = TInstDef (transformInstance st inst)
transformTopDecl st (TMutualDef ds) = TMutualDef (map (transformTopDecl st) ds)
transformTopDecl _ d = d

transformContract :: SigTable -> Contract Name -> Contract Name
transformContract st c = c {decls = map (transformContrDecl st) (decls c)}

transformContrDecl :: SigTable -> ContractDecl Name -> ContractDecl Name
transformContrDecl st (CFunDecl fd) = CFunDecl (transformFunDef st fd)
transformContrDecl st (CMutualDecl ds) = CMutualDecl (map (transformContrDecl st) ds)
transformContrDecl _ d = d

transformInstance :: SigTable -> Instance Name -> Instance Name
transformInstance st inst =
  inst {instFunctions = map (transformFunDef st) (instFunctions inst)}

transformFunDef :: SigTable -> FunDef Name -> FunDef Name
transformFunDef st fd =
  fd {funDefBody = transformBody st retTy (funDefBody fd)}
  where
    retTy = fromMaybe word (sigReturn (funSignature fd))

transformBody :: SigTable -> Ty -> Body Name -> Body Name
transformBody st retTy = map (transformStmt st retTy)

transformStmt :: SigTable -> Ty -> Stmt Name -> Stmt Name
-- Annotated let: transform RHS with the declared type as expected type.
-- Unannotated let (Nothing): leave unchanged — no type context available.
transformStmt st _ (Let ct n (Just ty) (Just e)) =
  Let ct n (Just ty) (Just (transformExp st ty e))
-- Return: use the function's return type as expected type.
transformStmt st retTy (Return e) =
  Return (transformExp st retTy e)
-- If: condition has unknown expected type (use word as safe default —
-- the condition is typically bool, not integer, so no literal coercion there;
-- sub-expression coercions happen via call-arg lookup inside transformExp).
transformStmt st retTy (If cond t f) =
  If (transformExp st word cond) (transformBody st retTy t) (transformBody st retTy f)
transformStmt st retTy (Block body) =
  Block (transformBody st retTy body)
transformStmt st retTy (Match es eqs) =
  Match es (map (\(ps, body) -> (ps, transformBody st retTy body)) eqs)
transformStmt st retTy (StmtExp e) =
  StmtExp (transformExp st retTy e)
-- Assignment: RHS expected type unknown, use word (no coercion).
transformStmt st _ (lhs := e) =
  lhs := transformExp st word e
-- Asm blocks: leave entirely unchanged.
transformStmt _ _ s = s

-- | Transform an expression given the expected type at this position.
transformExp :: SigTable -> Ty -> Exp Name -> Exp Name
-- Core rule: integer literal where `integer` is expected → insert coercion.
transformExp _ expectedTy (Lit (IntLit n))
  | expectedTy == integerTy =
      Call Nothing (Name "wordToInteger") [Lit (IntLit n)]
-- Call: ignore the outer expected type; coerce arguments using the callee's
-- declared parameter types from the signature table.
-- This handles both direct calls (integerAdd) and qualified class-method calls
-- (Num.fromInteger) via the qualified entries in the sig table.
transformExp st _ (Call mq f args) =
  Call mq f (transformCallArgs st f args)
-- Conditional: all three sub-expressions get default (word) expected type
-- since the ternary result type is not tracked here.
transformExp st _ (Cond c t e) =
  Cond (transformExp st word c) (transformExp st word t) (transformExp st word e)
-- Type ascription: propagate the annotated type as expected type.
transformExp st _ (TyExp e ty) =
  TyExp (transformExp st ty e) ty
-- Var, non-integer Lit, Con, Lam, FieldAccess: no coercion possible.
transformExp _ _ e = e

-- | Coerce call arguments using declared parameter types.
-- Unknown functions (not in table): leave args unchanged (conservative).
-- Polymorphic params (TyVar): param type ≠ integerTy → no coercion.
-- Arguments beyond the declared arity: left unchanged.
transformCallArgs :: SigTable -> Name -> [Exp Name] -> [Exp Name]
transformCallArgs st f args =
  case Map.lookup f st of
    Nothing ->
      -- Unknown function: recurse into args with word default (no literal coercion,
      -- but sub-call args are still handled via their own sig-table lookups).
      map (transformExp st word) args
    Just paramTys ->
      let coerced = zipWith (transformExp st) paramTys args
          rest = map (transformExp st word) (drop (length paramTys) args)
       in coerced ++ rest
