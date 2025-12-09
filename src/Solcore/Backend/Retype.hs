module Solcore.Backend.Retype where

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Primitives.Primitives
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Common.Pretty

type TcFunDef = FunDef Id
type TcExp = Exp Id

typeOfTcExp :: TcExp -> Ty
typeOfTcExp (Var i)               = idType i
typeOfTcExp (Con i [])            = idType i
typeOfTcExp e@(Con i args)          = go (idType i) args where
  go ty [] = ty
  go (_ :-> u) (a:as) = go u as
  go _ _ = error $ "typeOfTcExp: " ++ show e
typeOfTcExp (Lit (IntLit _))      = word --TyCon "Word" []
typeOfTcExp exp@(Call Nothing i args) = applyTo args funTy where
  funTy = idType i
  applyTo [] ty = ty
  applyTo (_:as) (_ :-> u) = applyTo as u
  applyTo _ _ = error $ concat [ "apply ", pretty i, " : ", pretty funTy
                       , "to", show $ map pretty args
                       , "\nIn:\n", show exp
                       ]
typeOfTcExp (Lam args body (Just tb))       = funtype tas tb where
  tas = map typeOfTcParam args
typeOfTcExp (Cond _ _ e) = typeOfTcExp e
typeOfTcExp (TyExp _ ty) = ty
typeOfTcExp e = error $ "typeOfTcExp: " ++ show e

typeOfTcStmt :: Stmt Id -> Ty
typeOfTcStmt (n := e) = unit
typeOfTcStmt (Let n _ _) = idType n
typeOfTcStmt (StmtExp e) = typeOfTcExp e
typeOfTcStmt (Return e) = typeOfTcExp e
typeOfTcStmt (Match _ ((pat, body):_)) = typeOfTcBody body

typeOfTcBody :: [Stmt Id] -> Ty
typeOfTcBody []    = unit
typeOfTcBody [s]   = typeOfTcStmt s
typeOfTcBody (_:b) = typeOfTcBody b

typeOfTcParam :: Param Id -> Ty
typeOfTcParam (Typed i t)  = idType i  -- seems better than t - see issue #6
typeOfTcParam (Untyped i) = idType i

typeOfTcSignature :: Signature Id -> Ty
typeOfTcSignature sig = funtype (map typeOfTcParam $ sigParams sig) (returnType sig) where
  returnType s = case sigReturn s of
    Just t -> t
    Nothing -> error ("no return type in signature of: " ++ show (sigName s))

schemeOfTcSignature :: Signature Id -> Scheme
schemeOfTcSignature sig@(Signature vs ps n args (Just rt))
  = case mapM getType args of
      Just ts -> Forall vs (ps :=> (funtype ts rt))
      Nothing -> error $ unwords ["Invalid instance member signature:", pretty sig]
    where
      getType (Typed _ t) = Just t
      getType _ = Nothing

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef sig _) = typeOfTcSignature sig

