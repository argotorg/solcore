module Solcore.Desugarer.DeriveClass
  ( deriveClassTopDecls,
  )
where

import Control.Monad (foldM, unless, when)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Solcore.Frontend.Syntax

-- | Derive the explicitly requested unary traits after Generic instances have
-- been added. Keeping this separate from automatic Generic derivation ensures
-- that phantom parameters also receive the requested trait constraint and
-- that duplicate requests reach the ordinary instance-overlap checker.
deriveClassTopDecls :: [DataTy] -> [TopDecl Name] -> Either String [TopDecl Name]
deriveClassTopDecls localData allDecls = do
  generated <- concat <$> mapM deriveData localData
  pure (allDecls ++ map TInstDef generated)
  where
    classes = [cls | TClassDef cls <- allDecls]
    deriveData dt = mapM (deriveTarget dt) (dataDerives dt)
    deriveTarget dt target = do
      cls <-
        maybe (invalid dt target "unknown trait") Right $
          find ((== target) . className) classes
      unless (null (paramsVar cls)) $
        invalid dt target "only single-parameter traits can be derived"
      when (inGenericContract (dataName dt) allDecls) $
        invalid dt target "a contract-local enum cannot capture generic contract parameters"
      methods <- mapM (deriveMethod allDecls dt cls) (signatures cls)
      pure
        Instance
          { instDefault = False,
            instVars = dataParams dt,
            instContext = [InCls target (TyVar variable) [] | variable <- dataParams dt],
            instName = target,
            paramsTy = [],
            mainTy = dataType dt,
            instFunctions = methods
          }

invalid :: DataTy -> Name -> String -> Either String a
invalid dt target reason =
  Left ("cannot derive trait '" ++ show target ++ "' for enum '" ++ show (dataName dt) ++ "': " ++ reason)

inGenericContract :: Name -> [TopDecl Name] -> Bool
inGenericContract target = any contains
  where
    contains (TContr (Contract _ parameters members)) =
      not (null parameters)
        && any (\member -> case member of CDataDecl dt -> dataName dt == target; _ -> False) members
    contains (TMutualDef declarations) = inGenericContract target declarations
    contains _ = False

dataType :: DataTy -> Ty
dataType dt = TyCon (dataName dt) (map TyVar (dataParams dt))

deriveMethod :: [TopDecl Name] -> DataTy -> Class Name -> Signature Name -> Either String (FunDef Name)
deriveMethod allDecls dt cls source = do
  let self = mainVar cls
      shadowed = self `elem` sigVars source
      isSelf ty = not shadowed && ty == TyVar self
      containsSelf ty = not shadowed && occurs self ty
      sourceTypes = mapMaybe paramType (sigParams source) ++ maybe [] (: []) (sigReturn source)
      failMethod :: String -> Either String result
      failMethod reason = invalid dt (className cls) ("method '" ++ show (sigName source) ++ "': " ++ reason)
  when (any (\ty -> containsSelf ty && not (isSelf ty)) sourceTypes) $
    failMethod "the trait parameter must occur directly, not inside another type"
  unless (all isTyped (sigParams source)) $
    failMethod "all parameters must have explicit types"
  -- A method's own binders are independent of both the trait's Self binder and
  -- the enum's binders, even when users choose the same source spelling.
  let renamedMethodVars =
        [TVar (Name ("$derive_method_" ++ show index)) | index <- [0 .. length (sigVars source) - 1]]
      methodRenaming = Map.fromList (zip (sigVars source) (map TyVar renamedMethodVars))
      selfReplacement
        | shadowed = methodRenaming
        | otherwise = Map.insert self (dataType dt) methodRenaming
      replace = replaceTy selfReplacement
      wrapperSig =
        source
          { sigVars = renamedMethodVars,
            sigContext = map (replacePred selfReplacement) (sigContext source),
            sigParams = map (replaceParam replace) (sigParams source),
            sigReturn = fmap replace (sigReturn source),
            sigReturnItems =
              [item {signatureReturnItemType = replace (signatureReturnItemType item)} | item <- sigReturnItems source]
          }
  body <-
    if null (dataConstrs dt)
      then emptyMethodBody allDecls failMethod isSelf (sigParams source)
      else do
        (genericName, representation) <- genericRepresentation allDecls dt (className cls)
        let convertArgument parameter = case parameter of
              Typed _ variable ty
                | isSelf ty ->
                    TyExp (classCall genericName "from" [Var variable]) representation
              _ -> Var (paramName parameter)
            delegatedResult =
              case sigReturn source of
                Just ty | isSelf ty -> representation
                Just ty -> replace ty
                Nothing -> TyCon (Name "()") []
            delegated =
              TyExp
                (classCall (className cls) (leafName (sigName source)) (map convertArgument (sigParams source)))
                delegatedResult
            result = case sigReturn source of
              Just ty | isSelf ty -> classCall genericName "to" [delegated]
              _ -> delegated
        pure [Return result]
  pure (FunDef False wrapperSig body)
  where
    paramType (Typed _ _ ty) = Just ty
    paramType Untyped {} = Nothing
    isTyped Typed {} = True
    isTyped Untyped {} = False

emptyMethodBody ::
  [TopDecl Name] ->
  (String -> Either String (Body Name)) ->
  (Ty -> Bool) ->
  [Param Name] ->
  Either String (Body Name)
emptyMethodBody allDecls failure isSelf parameters =
  case [sigName sig | TFunDef (FunDef _ sig _) <- allDecls, leafName (sigName sig) == "absurd", null (sigParams sig)] of
    absurdName : _ -> pure [Return (Call Nothing absurdName [])]
    [] -> case [variable | Typed _ variable ty <- parameters, isSelf ty] of
      variable : _ -> pure [Match [Var variable] []]
      [] -> failure "an empty enum method without an enum argument requires an in-scope absurd() function"

classCall :: Name -> String -> [Exp Name] -> Exp Name
classCall cls method = Call Nothing (QualName cls method)

leafName :: Name -> String
leafName (Name value) = value
leafName (QualName _ value) = value

occurs :: Tyvar -> Ty -> Bool
occurs variable (TyVar other) = variable == other
occurs variable (TyCon _ arguments) = any (occurs variable) arguments
occurs _ (Meta _) = False

replaceTy :: Map Tyvar Ty -> Ty -> Ty
replaceTy replacements ty@(TyVar variable) = Map.findWithDefault ty variable replacements
replaceTy replacements (TyCon constructor arguments) = TyCon constructor (map (replaceTy replacements) arguments)
replaceTy _ ty@(Meta _) = ty

replacePred :: Map Tyvar Ty -> Pred -> Pred
replacePred replacements (InCls cls primary arguments) =
  InCls cls (replaceTy replacements primary) (map (replaceTy replacements) arguments)
replacePred replacements (left :~: right) =
  replaceTy replacements left :~: replaceTy replacements right

replaceParam :: (Ty -> Ty) -> Param a -> Param a
replaceParam replace (Typed comptime variable ty) = Typed comptime variable (replace ty)
replaceParam _ parameter@Untyped {} = parameter

-- A manual Generic instance may deliberately choose a representation different
-- from the enum's structural representation, so consult the generated/manual
-- instances rather than recomputing the sum-of-products type here.
genericRepresentation :: [TopDecl Name] -> DataTy -> Name -> Either String (Name, Ty)
genericRepresentation allDecls dt target =
  case preferred of
    [(_, cls, representation)] -> Right (cls, representation)
    [] -> invalid dt target "no Generic instance supplies a representation"
    _ -> invalid dt target "multiple Generic instances supply a representation"
  where
    matching = mapMaybe matchInstance [inst | TInstDef inst <- allDecls, leafName (instName inst) == "Generic"]
    ordinary = filter (\(isDefault, _, _) -> not isDefault) matching
    preferred = if null ordinary then matching else ordinary
    matchInstance inst = do
      [representation] <- pure (paramsTy inst)
      replacements <- matchType (instVars inst) Map.empty (mainTy inst) (dataType dt)
      pure (instDefault inst, instName inst, replaceTy replacements representation)

matchType :: [Tyvar] -> Map Tyvar Ty -> Ty -> Ty -> Maybe (Map Tyvar Ty)
matchType bound replacements (TyVar variable) actual
  | variable `elem` bound =
      case Map.lookup variable replacements of
        Nothing -> Just (Map.insert variable actual replacements)
        Just previous | previous == actual -> Just replacements
        _ -> Nothing
matchType bound replacements (TyCon constructor arguments) (TyCon other actual)
  | constructor == other && length arguments == length actual =
      foldM (\current (patternTy, actualTy) -> matchType bound current patternTy actualTy) replacements (zip arguments actual)
matchType _ replacements patternTy actualTy
  | patternTy == actualTy = Just replacements
matchType _ _ _ _ = Nothing
