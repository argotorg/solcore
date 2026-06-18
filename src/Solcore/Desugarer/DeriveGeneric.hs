module Solcore.Desugarer.DeriveGeneric where

import Data.List (nub)
import Data.List.NonEmpty (toList)
import Solcore.Frontend.Syntax

-- Generate Generic instances for data types

deriveGenericTopDecls :: [DataTy] -> [TopDecl Name] -> Either String [TopDecl Name]
deriveGenericTopDecls localData allDecls
  | not (genericClassVisible allDecls) = Right allDecls
  | (n : _) <- conflicts               = Left (conflictError n)
  | otherwise                          = Right (allDecls ++ newInsts)
  where
    excluded  = pragmaExcluded allDecls
    hasInst   = existingGenericTypes allDecls
    conflicts = [ dataName dt
                | dt <- localData
                , dataName dt `elem` hasInst
                , dataName dt `notElem` excluded ]
    newInsts  =
      [ TInstDef (buildInstance dt)
        | dt <- localData,
          not (null (dataConstrs dt)),
          dataName dt `notElem` excluded,
          dataName dt `notElem` hasInst
      ]
    conflictError n =
      "type '" ++ show n ++ "' has a manual Generic instance " ++
      "but no 'pragma no-generic-instance-for " ++ show n ++ "'; " ++
      "add the pragma to suppress auto-derivation"

genericClassVisible :: [TopDecl Name] -> Bool
genericClassVisible = any isGenericClass
  where
    isGenericClass (TClassDef cls) = className cls == Name "Generic"
    isGenericClass _ = False

collectDataDefs :: [TopDecl Name] -> [DataTy]
collectDataDefs = concatMap go
  where
    go (TDataDef dt) = [dt]
    go (TContr (Contract _ _ ds)) = [dt | CDataDecl dt <- ds]
    go _ = []

existingGenericTypes :: [TopDecl Name] -> [Name]
existingGenericTypes = concatMap go
  where
    go (TInstDef inst)
      | instName inst == Name "Generic" = [tyConName (mainTy inst)]
    go _ = []
    tyConName (TyCon n _) = n
    tyConName _ = Name ""

pragmaExcluded :: [TopDecl Name] -> [Name]
pragmaExcluded = nub . concatMap go
  where
    go (TPragmaDecl (Pragma NoGenericInstanceFor (DisableFor names))) =
      toList names
    go _ = []

-- SOP representation type

unitTy :: Ty
unitTy = TyCon (Name "()") []

mkProdOf :: [Ty] -> Ty
mkProdOf [] = unitTy
mkProdOf [t] = t
mkProdOf (t : ts) = TyCon (Name "pair") [t, mkProdOf ts]

mkSumOf :: [Ty] -> Ty
mkSumOf [] = unitTy
mkSumOf [t] = t
mkSumOf (t : ts) = TyCon (Name "sum") [t, mkSumOf ts]

constrRep :: Constr -> Ty
constrRep (Constr _ []) = unitTy
constrRep (Constr _ [t]) = t
constrRep (Constr _ ts) = mkProdOf ts

sopRep :: DataTy -> Ty
sopRep dt = mkSumOf (map constrRep (dataConstrs dt))

-- Expression helpers

mkProdExp :: [Exp Name] -> Exp Name
mkProdExp [] = Con (Name "()") []
mkProdExp [e] = e
mkProdExp (e : es) = Con (Name "pair") [e, mkProdExp es]

applyInr :: Int -> Exp Name -> Exp Name
applyInr 0 e = e
applyInr n e = Con (Name "inr") [applyInr (n - 1) e]

wrapSumExp :: Int -> Int -> Exp Name -> Exp Name
wrapSumExp _ 1 inner = inner
wrapSumExp idx total inner
  | idx == total - 1 = applyInr (total - 1) inner
  | otherwise = applyInr idx (Con (Name "inl") [inner])

pairPat :: Pat Name -> Pat Name -> Pat Name
pairPat p1 p2 = PCon (Name "pair") [p1, p2]

mkProdPat :: [Name] -> Pat Name
mkProdPat [] = PCon (Name "()") []
mkProdPat [v] = PVar v
mkProdPat vs = foldr1 pairPat (map PVar vs)

applyPInr :: Int -> Pat Name -> Pat Name
applyPInr 0 p = p
applyPInr n p = PCon (Name "inr") [applyPInr (n - 1) p]

wrapSumPat :: Int -> Int -> Pat Name -> Pat Name
wrapSumPat _ 1 inner = inner
wrapSumPat idx total inner
  | idx == total - 1 = applyPInr (total - 1) inner
  | otherwise = applyPInr idx (PCon (Name "inl") [inner])

freshVarNames :: Int -> [Name]
freshVarNames n = [Name ("_gv" ++ show i) | i <- [0 .. n - 1]]

fromClause :: Int -> Int -> Constr -> Equation Name
fromClause idx total (Constr cname tys) =
  let vars = freshVarNames (length tys)
      pat = PCon cname (map PVar vars)
      prodExp = mkProdExp (map Var vars)
      sumExp = wrapSumExp idx total prodExp
   in ([pat], [Return sumExp])

fromBody :: DataTy -> Body Name
fromBody dt =
  let constrs = dataConstrs dt
      total = length constrs
   in [Match [Var (Name "_x")] (zipWith (\i c -> fromClause i total c) [0 ..] constrs)]

toClause :: Int -> Int -> Constr -> Equation Name
toClause idx total (Constr cname tys) =
  let vars = freshVarNames (length tys)
      prodPat = mkProdPat vars
      sumPat = wrapSumPat idx total prodPat
      conExp = Con cname (map Var vars)
   in ([sumPat], [Return conExp])

toBody :: DataTy -> Body Name
toBody dt =
  let constrs = dataConstrs dt
      total = length constrs
   in [Match [Var (Name "_r")] (zipWith (\i c -> toClause i total c) [0 ..] constrs)]

buildFrom :: DataTy -> FunDef Name
buildFrom dt = FunDef False sig (fromBody dt)
  where
    mainT = TyCon (dataName dt) (map TyVar (dataParams dt))
    repT = sopRep dt
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "from",
          sigParams = [Typed False (Name "_x") mainT],
          sigRetComptime = False,
          sigReturn = Just repT,
          sigPayable = False
        }

buildTo :: DataTy -> FunDef Name
buildTo dt = FunDef False sig (toBody dt)
  where
    mainT = TyCon (dataName dt) (map TyVar (dataParams dt))
    repT = sopRep dt
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "to",
          sigParams = [Typed False (Name "_r") repT],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }

buildInstance :: DataTy -> Instance Name
buildInstance dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [],
      instName = Name "Generic",
      paramsTy = [sopRep dt],
      mainTy = TyCon (dataName dt) (map TyVar (dataParams dt)),
      instFunctions = [buildFrom dt, buildTo dt]
    }
