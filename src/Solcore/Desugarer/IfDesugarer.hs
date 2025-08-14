module Solcore.Desugarer.IfDesugarer where

import Data.Generics

import Control.Monad.Identity

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv (primCtx)
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives


ifDesugarer :: CompUnit Id -> CompUnit Id
ifDesugarer cunit
  = let
       c1 = everywhere (mkT desugarTyBool) cunit
       c2 = everywhere (mkT desugarBoolCons) c1
       c3 = everywhere (mkT desugarBoolPat) c2
    in everywhere (mkT desugarStmt) c3

-- desugaring if stmts

desugarStmt :: Stmt Id -> Stmt Id
desugarStmt (Match es eqns)
  = Match es (map (\ (ps, bdy) -> (ps, map desugarStmt bdy)) eqns)
desugarStmt (If e bdy1 bdy2)
  = Match [e] [eqntrue, eqnfalse]
    where
      eqntrue = ([PCon (Id inlName (inlTy unit unit)) [PCon (Id "()" unit) []]], bdy1)
      eqnfalse = ([PCon (Id inrName (inrTy unit unit)) [PCon (Id "()" unit) []]], bdy2)
desugarStmt v = v

-- desugaring boolean data constructors

desugarBoolCons :: Exp Id -> Exp Id
desugarBoolCons (Con c es)
  | isBoolCon c = sumConsFor c
  | otherwise = Con c (map desugarBoolCons es)
desugarBoolCons (FieldAccess me v)
  = FieldAccess (desugarBoolCons <$> me) v
desugarBoolCons (Call me v es)
  = Call (desugarBoolCons <$> me) v (map desugarBoolCons es)
desugarBoolCons (Lam ps bdy ty)
  = Lam ps (everywhere (mkT desugarBoolCons) bdy) ty
desugarBoolCons (TyExp e t)
  = TyExp (desugarBoolCons e) t
desugarBoolCons (Var a) = Var a
desugarBoolCons (Lit l) = Lit l

desugarBoolPat :: Pat Id -> Pat Id
desugarBoolPat (PCon c ps)
  | isBoolCon c = sumPatFor c
  | otherwise = PCon c (map desugarBoolPat ps)
desugarBoolPat (PVar a) = PVar a
desugarBoolPat PWildcard = PWildcard
desugarBoolPat (PLit l) = PLit l

sumConsFor :: Id -> Exp Id
sumConsFor (Id n _)
  | n == trueName
    = Con (Id inlName (inlTy unit unit)) [Con (Id "()" unit) []]
  | n == falseName
    = Con (Id inrName (inrTy unit unit)) [Con (Id "()" unit) []]

sumPatFor :: Id -> Pat Id
sumPatFor (Id n _)
  | n == trueName
    = PCon (Id inlName (inlTy unit unit)) [PCon (Id "()" unit) []]
  | n == falseName
    = PCon (Id inrName (inrTy unit unit)) [PCon (Id "()" unit) []]

isBoolCon :: Id -> Bool
isBoolCon (Id n _) = n `elem` [trueName, falseName]

-- desugaring the boolean type constructor

desugarTyBool :: Ty -> Ty
desugarTyBool t@(TyCon n [])
  | n == boolName = sumTy unit unit
  | otherwise = t
desugarTyBool (TyCon n ts)
  = TyCon n (map desugarTyBool ts)
desugarTyBool t = t

