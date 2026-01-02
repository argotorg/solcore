-- {-# LANGUAGE DefaultSignatures #-}
{-|
Module: Solcore.Backend.Specialise
Description: Monomorphization pass - eliminates polymorphism via specialization

This module implements whole-program specialization (monomorphization) that transforms
polymorphic and type class-overloaded code into concrete, monomorphic definitions.

= Algorithm Overview

The specialization process:

  1. Builds a resolution table mapping (name, type) → definition
  2. Analyzes call sites to discover needed type instantiations
  3. Creates specialized versions with mangled names (e.g., map$word, map$bool)
  4. Resolves type class instances to concrete implementations
  5. Recursively specializes all called functions

= Pipeline Position

Runs after type checking and defunctionalization, but before Core emission.
Requires whole-program analysis (must see all code at once).

Input: Typed AST with polymorphic functions and type class constraints
Output: Monomorphic AST with all type variables eliminated

-}
module Solcore.Backend.Specialise(specialiseCompUnit) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List(intercalate)
import qualified Data.Map as Map

import Common.Monad
import Solcore.Backend.Retype
import Solcore.Backend.SpecMonad
import Solcore.Desugarer.IfDesugarer(desugaredBoolTy)
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding(name, decls)
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv)
import Solcore.Primitives.Primitives

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- | The entry point name for contract runtime code
entryPointName :: Name
entryPointName = Name "main"

-- | The constructor function name
constructorName :: Name
constructorName = Name "constructor"

-- | Built-in revert primitive name
revertBuiltin :: Name
revertBuiltin = Name "revert"

-- | Placeholder type variable used for initial resolution lookups
anyTypePlaceholder :: Ty
anyTypePlaceholder = TyVar (TVar (Name "any"))

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

specialiseCompUnit :: CompUnit Id -> Bool -> TcEnv -> IO (CompUnit Id)
specialiseCompUnit compUnit debugp env = runSM debugp env do
    addGlobalResolutions compUnit
    topDecls <- concat <$> forM (contracts compUnit) specialiseTopDecl
    return $ compUnit { contracts = topDecls }

-------------------------------------------------------------------------------
-- Resolution Table Building
-------------------------------------------------------------------------------
-- Build a table mapping (name, type) to function definitions
-- This enables looking up which definition to use when specializing a call

-- | Type class for declarations that can contribute to the resolution table
class HasResolutions decl where
    addResolutions :: decl -> SM ()

addGlobalResolutions :: CompUnit Id -> SM ()
addGlobalResolutions compUnit = forM_ (contracts compUnit) addResolutions

instance HasResolutions (TopDecl Id) where
    addResolutions (TInstDef inst) = addInstResolutions inst
    addResolutions (TFunDef fd) = addFunDefResolution fd
    addResolutions (TDataDef dt) = addData dt
    addResolutions (TMutualDef decls) = mapM_ addResolutions decls
    addResolutions _ = return ()

instance HasResolutions (ContractDecl Id) where
    addResolutions (CFunDecl fd) = addFunDefResolution fd
    addResolutions (CDataDecl dt) = addData dt
    addResolutions (CMutualDecl decls) = mapM_ addResolutions decls
    addResolutions _ = return ()

addInstResolutions :: Instance Id -> SM ()
addInstResolutions inst = forM_ (instFunctions inst) (addMethodResolution (instName inst) (mainTy inst))

addFunDefResolution :: FunDef Id -> SM ()
addFunDefResolution fd = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  addResolution name funType fd
  debug ["+ addDeclResolution: ", show name, " : ", pretty funType]

addMethodResolution :: Name -> Ty -> TcFunDef -> SM ()
addMethodResolution cname ty fd = do
  let sig = funSignature fd
  let name = sigName sig
  let qname = case name of
        QualName{} -> name
        Name s -> QualName cname s
  let name' = specName qname [ty]
  let funType = typeOfTcFunDef fd
  let fd' = FunDef sig{sigName = name'} (funDefBody fd)
  addResolution qname funType fd'
  debug ["+ addMethodResolution: ", show qname, " / ", show name', " : ", pretty funType]

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract _name _args cdecls) =
  forM_ cdecls addResolutions

-------------------------------------------------------------------------------
-- Top-Level Specialization
-------------------------------------------------------------------------------

specialiseTopDecl :: TopDecl Id -> SM [TopDecl Id]
specialiseTopDecl (TContr contract@(Contract name args decls)) = withLocalState $ do
    addContractResolutions contract
    runtimeDecls <- specialiseRuntimeDecls
    deployDecls <- specialiseDeployerCode decls
    return [TContr (Contract name args (deployDecls ++ runtimeDecls))]
  where
    -- | Specializes all runtime entry points (public methods)
    specialiseRuntimeDecls :: SM [ContractDecl Id]
    specialiseRuntimeDecls = withLocalState $ do
        forM_ [entryPointName] specEntry  -- Eventually all public methods
        collectSpecialisedDecls

    -- | Specializes the constructor (deployer code) if present
    specialiseDeployerCode :: [ContractDecl Id] -> SM [ContractDecl Id]
    specialiseDeployerCode contractDecls = do
        -- Clear the specialization table for deployer code
        modify (\st -> st { specTable = emptyTable })
        case findConstructor contractDecls of
            Just c -> specialiseConstructorDecls c
            Nothing -> pure []

    -- | Specializes a constructor and its dependencies
    specialiseConstructorDecls :: Constructor Id -> SM [ContractDecl Id]
    specialiseConstructorDecls c = withLocalState $ do
        _cname' <- specConstructor c
        depDecls <- collectSpecialisedDecls
        -- Use mutual to group constructor with its dependencies
        pure [CMutualDecl depDecls]

    -- | Collects all specialized functions and data types from the current state
    collectSpecialisedDecls :: SM [ContractDecl Id]
    collectSpecialisedDecls = do
        st <- gets specTable
        dt <- gets spDataTable
        let dataDecls = map (CDataDecl . snd) (Map.toList dt)
        let funDecls = map (CFunDecl . snd) (Map.toList st)
        pure (dataDecls ++ funDecls)

-- Keep datatype defs intact
specialiseTopDecl d@TDataDef{} = pure [d]
-- Drop all toplevel decls that are not contracts - we don't need them after specialization
specialiseTopDecl _ = pure []

findConstructor :: [ContractDecl Id] -> Maybe (Constructor Id)
findConstructor = foldr (\d -> (getConstructor d <|>)) Nothing

getConstructor :: ContractDecl Id -> Maybe (Constructor Id)
getConstructor (CConstrDecl c) = Just c
getConstructor _ = Nothing


specEntry :: Name -> SM ()
specEntry name = withLocalState do
    mres <- lookupResolution name anyTypePlaceholder
    case mres of
      Just (fd, ty, subst) -> do
        debug ["< resolution: ", show name, " : ", pretty ty, "@", pretty subst]
        void(specFunDef fd)
      Nothing -> do
        warns ["!! Warning: no resolution found for ", show name]

specConstructor :: Constructor Id -> SM Name
specConstructor (Constructor [] body) = do
  let sig = Signature [] [] constructorName [] (Just unit)
  let fd = FunDef sig body
  specFunDef fd
specConstructor _ = error "Unsupported constructor"

-------------------------------------------------------------------------------
-- Expression Specialization
-------------------------------------------------------------------------------

-- | `specExp` specialises an expression to given type
specExp :: TcExp -> Ty -> SM TcExp
specExp (Call Nothing i args) ty = do
  -- debug ["> specExp (Call): ", pretty e, " : ", pretty (idType i), " ~> ", pretty ty]
  (i', args') <- specCall i args ty
  let e' = Call Nothing i' args'
  -- debug ["< specExp (Call): ", pretty e']
  return e'
specExp (Con i es) ty = do
  -- let t = typeOfTcExp e
  -- debug ["> specConApp: ", pretty e, " : ", pretty t, " ~> ", pretty ty]
  (i' , es') <- specConApp i es ty
  let e' = Con i' es'
  return e'
specExp (Cond e1 e2 e3) ty = do
  e1' <- specExp e1 desugaredBoolTy
  e2' <- specExp e2 ty
  e3' <- specExp e3 ty
  pure (Cond e1' e2' e3')
specExp (Var (Id n _t)) ty = pure (Var (Id n ty))
specExp e@FieldAccess{} _ty =
    error $ unlines
        [ "Specialization failed: FieldAccess should have been desugared"
        , "Expression: " ++ pretty e
        , ""
        , "This is a compiler bug - FieldAccess expressions should not reach specialization."
        , "Check that field access desugaring ran before specialization in the pipeline."
        ]
specExp (TyExp e1 _) ty = specExp e1 ty
specExp e _ty = atCurrentSubst e -- FIXME

specConApp :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
-- specConApp i@(Id n conTy) [] ty = pure (i, [])
specConApp i@(Id _n conTy) args ty = do
  subst <- getSpSubst
  let argTypes = map typeOfTcExp args
  let argTypes' = applytv subst argTypes
  let i' = applytv subst i
  let typedArgs = zip args argTypes'
  args' <- forM typedArgs (uncurry specExp)
  let conTy' = foldr (:->) ty argTypes'
  debug ["> specConApp: ", prettyId i, " : ", pretty conTy, " ~> ", prettyId i', " : ", pretty conTy']
  debug ["< specConApp: ", prettyConApp i args,  " ~> ", prettyConApp i' args']
  return (i', args')

-- | Specializes a function call to concrete types
-- Given actual arguments and the expected result type
specCall :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
specCall i@(Id name _e) args ty
  | isBuiltin name = pure (i, args)  -- Built-ins don't need specialization
  | otherwise = do
      -- Apply current substitution
      i' <- atCurrentSubst i
      expectedResultTy <- atCurrentSubst ty

      -- Specialize arguments
      let argTypes = map typeOfTcExp args
      argTypes' <- atCurrentSubst argTypes
      specializedArgs <- zipWithM specExp args argTypes'

      -- Compute expected function type
      let funType = foldr (:->) expectedResultTy argTypes'
      debug ["> specCall: ", show (idName i'), " : ", pretty funType]

      -- Look up and apply resolution
      resolveAndSpecialize (idName i') funType specializedArgs i args
  where
    _ty = _ty  -- Suppress unused variable warning

-- | Check if a name is a built-in primitive that doesn't need specialization
isBuiltin :: Name -> Bool
isBuiltin name = name == revertBuiltin

-- | Resolves a function name to its definition and specializes it
resolveAndSpecialize :: Name -> Ty -> [TcExp] -> Id -> [TcExp] -> SM (Id, [TcExp])
resolveAndSpecialize name funType specializedArgs originalId originalArgs = do
    mres <- lookupResolution name funType
    case mres of
        Just (fd, fty, substitution) -> applyResolution fd fty substitution
        Nothing -> handleMissingResolution name funType originalId specializedArgs
  where
    -- Apply a found resolution to specialize the function
    applyResolution :: TcFunDef -> Ty -> TVSubst -> SM (Id, [TcExp])
    applyResolution fd fty phi = do
        debug ["< resolution: ", show name, "~>", shortName fd, " : ", pretty funType, "@", pretty phi]
        extSpSubst phi
        subst <- getSpSubst
        let instantiatedType = applytv subst fty
        ensureClosed instantiatedType (Call Nothing originalId originalArgs) subst
        specializedName <- specFunDef fd
        debug ["< specCall: ", pretty specializedName]
        finalArgs <- atCurrentSubst specializedArgs
        return (Id specializedName instantiatedType, finalArgs)

    -- Handle the case where no resolution is found
    handleMissingResolution :: Name -> Ty -> Id -> [TcExp] -> SM (Id, [TcExp])
    handleMissingResolution n ft i args = do
        void $ panics
          [ "Specialization failed: No resolution found for function call"
          , "Function: " ++ show n
          , "Required type: " ++ pretty ft
          , ""
          , "Possible causes:"
          , "  - Function is not defined or not in scope"
          , "  - Type mismatch between call site and definition"
          , "  - Missing instance for type class constraint"
          , ""
          , "Check that the function is defined and that type inference is correct."
          ]
        return (i, args)

-------------------------------------------------------------------------------
-- Function Specialization
-------------------------------------------------------------------------------

-- | Specializes a function definition to concrete types
--
-- Algorithm:
--   1. Rename bound type variables to avoid capture
--   2. Apply current substitution to eliminate type variables
--   3. Generate mangled name based on concrete types (e.g., map$word)
--   4. Check if specialization already exists (memoization)
--   5. If not, create placeholder (breaks recursive loops), specialize body, record result
--
-- Returns the mangled name of the specialized function
specFunDef :: TcFunDef -> SM Name
specFunDef originalDef = withLocalState $ do
    -- Step 1: Rename bound type variables to avoid capture
    (renamedDef, renaming) <- renametv originalDef
    let originalSig = funSignature originalDef
    let renamedSig = funSignature renamedDef

    -- Step 2: Apply renaming to current substitution
    subst <- renameSubst renaming <$> getSpSubst
    putSpSubst subst
    debug ["> specFunDef raw input: ", pretty originalSig,
           " renaming=", pretty renaming, " subst=", pretty subst]

    -- Step 3: Eliminate type variables and type class context
    let monomorphicSig = applytv subst renamedSig { sigVars = [], sigContext = [] }
    let name = sigName renamedSig
    let funType = typeOfTcFunDef renamedDef

    -- Step 4: Compute concrete type instantiation
    let freeTypeVars = freetv funType
    let concreteTypes = applytv subst (map TyVar freeTypeVars)
    debug ["> specFunDef ", pretty name, " : ", pretty renamedSig,
           " concreteTypes=", prettys concreteTypes, " subst=", pretty subst]

    -- Step 5: Generate mangled name (e.g., map$word$bool)
    let specializedName = specName name concreteTypes
    let specializedType = applytv subst funType

    -- Step 6: Check memoization table
    memoizedSpec <- lookupSpecialisation specializedName
    case memoizedSpec of
        Just _ -> return specializedName  -- Already specialized
        Nothing -> createNewSpecialization specializedName monomorphicSig renamedDef specializedType
  where
    -- Create and record a new specialization
    createNewSpecialization :: Name -> Signature Id -> TcFunDef -> Ty -> SM Name
    createNewSpecialization name sig def ty = do
        -- Add placeholder first to break infinite recursion in mutually recursive functions
        let placeholder = FunDef sig { sigName = name } []
        addSpecialisation name placeholder

        -- Recursively specialize the function body
        specializedBody <- specBody (funDefBody def)
        let specializedDef = FunDef sig { sigName = name } specializedBody

        debug ["+ specFunDef: adding specialisation ", show name, " : ", pretty ty]
        addSpecialisation name specializedDef
        return name

-------------------------------------------------------------------------------
-- Statement Specialization
-------------------------------------------------------------------------------

specBody :: [Stmt Id] -> SM [Stmt Id]
specBody = mapM specStmt

-- | Ensures that a type is closed (has no free type variables)
-- All types must be fully concrete after specialization for Yul code generation
ensureClosed :: Pretty a => Ty -> a -> TVSubst ->  SM ()
ensureClosed ty ctxt subst = do
  let tvs = freetv ty
  unless (null tvs) $ panics
    [ "Specialization failed: Type still contains free type variables"
    , "Context: " ++ pretty ctxt
    , "Type: " ++ pretty ty
    , "Free type variables: " ++ show tvs
    , "Current substitution: " ++ pretty subst
    , ""
    , "This indicates incomplete specialization - all types must be concrete."
    , "Check that all polymorphic functions have been properly instantiated."
    ]
{-
  let mvs = mv ty
  unless (null mvs) $ panics
    [ "Specialization failed: Type still contains meta variables"
    , "Context: " ++ pretty ctxt
    , "Type: " ++ pretty ty
    , "Meta variables: " ++ show mvs
    , "Current substitution: " ++ pretty subst
    ]
-}

specStmt :: Stmt Id -> SM(Stmt Id)
specStmt stmt@(Return e) = do
  subst <- getSpSubst
  let ty = typeOfTcExp e
  let ty' = applytv subst ty
  ensureClosed ty' stmt subst
  -- debug ["> specExp (Return): ", pretty e," : ", pretty ty, " ~> ", pretty ty']
  e' <- specExp e ty'
  -- debug ["< specExp (Return): ", pretty e']
  return $ Return e'

specStmt (Match exps alts) = specMatch exps alts

specStmt stmt@(Var i := e) = do
  subst <- getSpSubst
  i' <- atCurrentSubst i
  let ty' = idType i'
  debug ["> specStmt (:=): ", pretty i, " : ", pretty (idType i)
        , " @ ", pretty subst, "~>'", pretty ty']
  ensureClosed ty' stmt subst
  e' <- specExp e ty'
  debug ["< specExp (:=): ", pretty e']
  return $ Var i' := e'

specStmt stmt@(Let i mty mexp) = do
  subst <- getSpSubst
  debug ["> specStmt (Let): ", pretty i, " : ", pretty (idType i), " @ ", pretty subst]
  i' <- atCurrentSubst i
  let ty' = idType i'
  ensureClosed ty' stmt subst
  mty' <- atCurrentSubst mty
  case mexp of
    Nothing -> return $ Let i' mty' Nothing
    Just e -> Let i' mty' . Just <$> specExp e ty'

specStmt (StmtExp e) = do
  ty <- atCurrentSubst (typeOfTcExp e)
  e' <- specExp e ty
  return $ StmtExp e'

specStmt (Asm ys) = pure (Asm ys)
specStmt stmt = errors
  [ "Specialization failed: Unsupported statement type"
  , "Statement: " ++ show stmt
  , ""
  , "This statement type has not been implemented in the specializer."
  ]

specMatch :: [Exp Id] -> [([Pat Id], [Stmt Id])] -> SM (Stmt Id)
specMatch exps alts = do
  exps' <- specScruts exps
  alts' <- forM alts specAlt
  return $ Match exps' alts'
  where
    specAlt (pat, body) = do
      body' <- specBody body
      pat' <- atCurrentSubst pat
      return (pat', body')
    specScruts = mapM specScrut
    specScrut e = do
      ty <- atCurrentSubst (typeOfTcExp e)
      e' <- specExp e ty
      return e'

-------------------------------------------------------------------------------
-- Name Mangling
-------------------------------------------------------------------------------
-- Generate unique names for specialized functions by encoding type arguments
-- in the function name. This allows multiple specialized versions to coexist.

-- | Generate a unique mangled name for a specialized function
--
-- Examples:
--   @map + [] → "map"@           (no type arguments, no mangling)
--   @map + [word] → "map$word"@
--   @foo + [word, bool] → "foo$word_bool"@
--   @Bar.baz + [unit] → "Bar_baz$unit"@
--   @Pair + [word, bool] → "Pair$word_bool"@
specName :: Name -> [Ty] -> Name
specName n [] = Name $ flattenQual n
specName n ts = Name $ flattenQual n ++ "$" ++ intercalate "_" (map mangleTy ts)

-- | Flatten a qualified name into a string, replacing dots with underscores
--
-- Examples:
--   @Name "foo" → "foo"@
--   @QualName (Name "Bar") "baz" → "Bar_baz"@
--   @QualName (QualName (Name "A") "B") "C" → "A_B_C"@
flattenQual :: Name -> String
flattenQual (Name n) = n
flattenQual (QualName n s) = flattenQual n ++ "_" ++ s

-- | Mangle a type into a string suitable for name mangling
--
-- Type variables are omitted (should not appear after substitution).
-- Capitalization is preserved from the original type names.
--
-- Examples:
--   @word → "word"@
--   @bool → "bool"@
--   @() → "unit"@
--   @List<word> → "ListLwordJ"@
--   @Pair<word, bool> → "PairLword_boolJ"@
--   @Map<word, List<bool>> → "MapLword_ListLboolJJ"@
--
-- The delimiters L and J mark the beginning and end of type argument lists,
-- allowing nested generic types to be unambiguously encoded.
mangleTy :: Ty -> String
mangleTy (TyVar _) = ""  -- Type vars should be eliminated by substitution
mangleTy (TyCon (Name "()") []) = "unit"
mangleTy (TyCon (Name n) []) = n
mangleTy (TyCon n ts) = flattenQual n ++ embrace mantys
  where
    mantys = filter (not . null) (map mangleTy ts)
    embrace [] = ""
    embrace xs = "L" ++ intercalate "_" xs ++ "J"  -- L...J delimiters
mangleTy t = error $ "Specialise: mangleTy not implemented for " ++ show t

-------------------------------------------------------------------------------
-- Pretty Printing Utilities
-------------------------------------------------------------------------------

prettyId :: Id -> String
prettyId = render . pprId

pprId :: Id -> Doc
pprId (Id n t@TyVar{}) = ppr n <> text "@" <> ppr t
pprId (Id n t@TyCon{}) = ppr n <> "@" <> ppr t
pprId (Id n t) = ppr n <> text "@" <> parens(ppr t)

pprConApp :: Id -> [TcExp] -> Doc
pprConApp i args = pprId i <> brackets (commaSepList args)

prettyConApp :: Id -> [TcExp] -> String
prettyConApp i args = render (pprConApp i args)
