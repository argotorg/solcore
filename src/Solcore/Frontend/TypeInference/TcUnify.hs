module Solcore.Frontend.TypeInference.TcUnify where

import Common.Pretty
import Control.Monad.Except
import Data.List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Solcore.Diagnostics
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcSubst

-- standard unification machinery

varBind :: (MonadError CompilerError m) => MetaTv -> Ty -> m Subst
varBind v t
  | t == Meta v = return mempty
  | v `elem` mv t = infiniteTyErr v t
  | otherwise = do
      return (v +-> t)

isTyCon :: Ty -> Bool
isTyCon (TyCon _ _) = True
isTyCon _ = False

-- type matching

class Match a where
  match :: (MonadError CompilerError m) => a -> a -> m Subst

instance Match Ty where
  match (TyCon n ts) (TyCon n' ts')
    | n == n' = match ts ts'
  match t1@(Meta v) t
    | t1 == t = pure mempty
    | otherwise = do
        pure (v +-> t)
  match (TyVar v1) (TyVar v2)
    | isBound v1 || isBound v2 =
        boundVariablesErr [v1, v2]
    | v1 == v2 = pure mempty
  match t1 t2 = typesNotMatch t1 t2

instance (Pretty a, Match a) => Match [a] where
  match [] [] = pure mempty
  match (t : ts) (t' : ts') =
    do
      s1 <- match t t'
      s2 <- match ts ts'
      merge s1 s2
  match ts ts' = typesMatchListErr (map pretty ts) (map pretty ts')

instance Match Pred where
  match (InCls n t ts) (InCls n' t' ts')
    | n == n' = match (t : ts) (t' : ts')
    | otherwise =
        structuredUnifyError
          "SC0210"
          "classes do not match"
          ["left class: " ++ pretty n, "right class: " ++ pretty n']
          []
  match p1 p2 =
    structuredUnifyError
      "SC0211"
      "predicates do not match"
      ["left predicate: " ++ pretty p1, "right predicate: " ++ pretty p2]
      []

instance (HasType a, Match a) => Match (Qual a) where
  match (ps :=> t) (ps' :=> t') =
    do
      s1 <- match t t'
      s2 <- match (apply s1 ps) (apply s1 ps')
      merge s1 s2

-- most general unifier

class MGU a where
  mgu :: (MonadError CompilerError m) => a -> a -> m Subst

instance (HasType a, MGU a, Pretty a) => MGU [a] where
  mgu ts1 ts2
    | length ts1 == length ts2 = solve (zip ts1 ts2) mempty
    | otherwise = typesMguListErr ts1 ts2

instance MGU Ty where
  mgu (TyCon n ts) (TyCon n' ts')
    | n == n' && length ts == length ts' =
        solve (zip ts ts') mempty
  mgu (TyVar v) (TyVar v')
    | isBound v || isBound v' =
        boundVariablesErr [v, v']
    | v == v' = pure mempty
  mgu (Meta _) (TyVar v@(TVar _)) =
    boundVariablesErr [v]
  mgu (TyVar v@(TVar _)) (Meta _) =
    boundVariablesErr [v]
  mgu (Meta v) t = varBind v t
  mgu t (Meta v) = varBind v t
  mgu t1 t2 = typesDoNotUnify t1 t2

instance MGU Pred where
  mgu p1@(InCls n t ts) p2@(InCls n' t' ts')
    | n == n' = mgu (t : ts) (t' : ts')
    | otherwise =
        structuredUnifyError
          "SC0212"
          "predicates do not unify"
          ["left predicate: " ++ pretty p1, "right predicate: " ++ pretty p2]
          []
  mgu (t1 :~: t2) (t1' :~: t2') =
    mgu [t1, t2] [t1', t2']
  mgu p1 p2 =
    structuredUnifyError
      "SC0212"
      "predicates do not unify"
      ["left predicate: " ++ pretty p1, "right predicate: " ++ pretty p2]
      []

instance (HasType a, MGU a) => MGU (Qual a) where
  mgu (ps :=> t) (ps' :=> t') =
    do
      s <- mgu (sort ps) (sort ps')
      mgu (apply s t) (apply s t')

solve :: (MonadError CompilerError m, MGU a, HasType a) => [(a, a)] -> Subst -> m Subst
solve [] s = pure s
solve ((t1, t2) : ts) s =
  do
    s1 <- mgu (apply s t1) (apply s t2)
    s2 <- solve ts s1
    pure (s2 <> s1)

unifyTypes :: (MonadError CompilerError m) => [Ty] -> [Ty] -> m Subst
unifyTypes ts ts' = solve (zip ts ts') mempty

unifyAllTypes :: (MonadError CompilerError m) => [Ty] -> m Subst
unifyAllTypes [] = pure mempty
unifyAllTypes (t : ts) =
  do
    s1 <- unifyAllTypes ts
    s2 <- mgu (apply s1 t) (apply s1 t)
    pure (s2 <> s1)

-- composition operator for matching

merge :: (MonadError CompilerError m) => Subst -> Subst -> m Subst
merge s1@(Subst p1) s2@(Subst p2) =
  if agree
    then pure (Subst (Map.union p1 p2))
    else mergeError disagree
  where
    overlap = Set.toList (Map.keysSet p1 `Set.intersection` Map.keysSet p2)
    disagree = foldr step [] overlap
    step v ac
      | (apply s1 (Meta v)) == (apply s2 (Meta v)) = ac
      | otherwise = (apply s1 (Meta v), apply s2 (Meta v)) : ac
    agree =
      all
        (\v -> (apply s1 (Meta v)) == (apply s2 (Meta v)))
        overlap

mergeError :: (MonadError CompilerError m) => [(Ty, Ty)] -> m a
mergeError ts =
  structuredUnifyError
    "SC0213"
    "substitutions assign incompatible types"
    ss
    []
  where
    ss = map (("conflict: " ++) . go) ts
    go (x, y) = pretty x ++ " with " ++ pretty y

-- basic error messages

infiniteTyErr :: (MonadError CompilerError m) => MetaTv -> Ty -> m a
infiniteTyErr v t =
  structuredUnifyError
    "SC0214"
    "cannot construct infinite type"
    ["meta variable: " ++ pretty (metaName v), "type: " ++ pretty t]
    []

typesNotMatch :: (MonadError CompilerError m) => Ty -> Ty -> m a
typesNotMatch t1 t2 =
  typeMismatchDiagnostic "types do not match" t1 t2

typesMatchListErr :: (MonadError CompilerError m) => [String] -> [String] -> m a
typesMatchListErr ts ts' =
  structuredUnifyError
    "SC0215"
    "type lists do not match"
    ["left types: " ++ prettys ts, "right types: " ++ prettys ts']
    []

typesMguListErr :: (MonadError CompilerError m, Pretty t) => [t] -> [t] -> m a
typesMguListErr ts ts' =
  structuredUnifyError
    "SC0216"
    "type lists do not unify"
    ["left types: " ++ prettys ts, "right types: " ++ prettys ts']
    []

typesDoNotUnify :: (MonadError CompilerError m) => Ty -> Ty -> m a
typesDoNotUnify t1 t2 =
  typeMismatchDiagnostic "types do not unify" t1 t2

typeMismatchDiagnostic :: (MonadError CompilerError m) => String -> Ty -> Ty -> m a
typeMismatchDiagnostic message t1 t2 =
  throwError $
    diagnosticCompilerError $
      Diagnostic
        { diagnosticSeverity = Error,
          diagnosticCode = Just (DiagnosticCode "SC0201"),
          diagnosticMessage = message ++ ": " ++ pretty t1 ++ " and " ++ pretty t2,
          diagnosticLabels = [],
          diagnosticNotes =
            [ "left type: " ++ pretty t1,
              "right type: " ++ pretty t2
            ],
          diagnosticHelp = []
        }

boundVariablesErr :: (MonadError CompilerError m) => [Tyvar] -> m a
boundVariablesErr ts =
  structuredUnifyError
    "SC0217"
    "bound variable escaped unification"
    ["bound variables: " ++ prettys ts]
    ["report this as a compiler bug if it is reachable from source code"]

structuredUnifyError :: (MonadError CompilerError m) => String -> String -> [String] -> [String] -> m a
structuredUnifyError code message notes help =
  throwError $
    diagnosticCompilerError $
      Diagnostic
        { diagnosticSeverity = Error,
          diagnosticCode = Just (DiagnosticCode code),
          diagnosticMessage = message,
          diagnosticLabels = [],
          diagnosticNotes = notes,
          diagnosticHelp = help
        }
