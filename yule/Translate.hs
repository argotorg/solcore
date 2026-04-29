{-# LANGUAGE OverloadedStrings #-}

module Translate where

import Builtins
import Common.Pretty
import Data.Map qualified as Map
import Data.String
import GHC.Stack
import Language.Hull hiding (Name)
import Language.Hull qualified as Hull
import Language.Yul
import Solcore.Frontend.Syntax.Name
import TM

genExpr :: Expr -> TM ([YulStmt], Location)
genExpr (EWord n) = pure ([], LocWord n)
genExpr (EBool b) = pure ([], LocBool b)
genExpr (EVar name) = do
  loc <- lookupVar name
  pure ([], loc)
genExpr (EPair e1 e2) = do
  (stmts1, loc1) <- genExpr e1
  (stmts2, loc2) <- genExpr e2
  pure (stmts1 ++ stmts2, LocSeq [loc1, loc2])
genExpr (EFst e) = do
  (stmts, loc) <- genExpr e
  case loc of
    LocPair l _ -> pure (stmts, l)
    _ -> error "EFst: type mismatch"
genExpr (ESnd e) = do
  (stmts, loc) <- genExpr e
  case loc of
    LocPair _ r -> pure (stmts, r)
    _ -> error "ESnd: type mismatch"
genExpr (EInl (TSum _ r) e) = do
  (stmts, loc) <- genExpr e
  let loc' = loc `padToSize` sizeOf r
  pure (stmts, LocSeq [LocBool False, loc'])
genExpr (EInr (TSum _ r) e) = do
  (stmts, loc) <- genExpr e
  let loc' = loc `paddedTo` r
  pure (stmts, LocSeq [LocBool True, loc'])
genExpr (EInl (TNamed _ t) e) = genExpr (EInl t e)
genExpr (EInr (TNamed _ t) e) = genExpr (EInr t e)
genExpr (EInK k (TSumN ts) e) = do
  (stmts, loc) <- genExpr e
  let maxsize = maximum (map sizeOf ts)
  let loc' = loc `padToSize` maxsize
  pure (stmts, LocSeq [LocWord (fromIntegral k), loc'])
genExpr EUnit = pure ([], LocUnit)
genExpr (ECall name args) = do
  (argCodes, argLocs) <- unzip <$> mapM genExpr args
  let argsCode = concat argCodes
  let yulArgs = concatMap flattenRhs argLocs
  funInfo <- lookupFun name
  (resultCode, resultLoc) <- hullAlloc (fun_result funInfo)
  let callExpr = YCall (yulFunName name) yulArgs
  let callCode = case sizeOf (resultLoc) of -- handle void functions
        0 -> [YExp callExpr]
        _ -> [YAssign (flattenLhs resultLoc) callExpr]
  pure (argsCode ++ resultCode ++ callCode, resultLoc)
genExpr e@(ECond ty cond e1 e2) = do
  debug ["genExpr: ", show e]
  (resultCode, resultLoc) <- hullAlloc ty
  (condCode, condLoc) <- genExpr cond
  -- Bools are complex(False ~ inl ()) to get something we can switch on
  let tag = normalizeLoc condLoc
  debug ["tag = ", show tag]
  (code1, loc1) <- genExpr e1
  (code2, loc2) <- genExpr e2
  let preCode = resultCode <> condCode <> code1 <> code2
  let yulDefault = Just (copyLocs resultLoc loc1)
  let zeroCode = copyLocs resultLoc loc2
  let switch = [YSwitch (loadLoc tag) [(YulNumber 0, zeroCode)] yulDefault]
  pure (preCode <> switch, resultLoc)
genExpr e = error ("genExpr: not implemented for " ++ show e)

yulFunName :: Hull.Name -> Name
yulFunName = fromString . ("usr$" ++)

yulVarName :: Hull.Name -> Name
yulVarName = fromString

flattenRhs :: Location -> [YulExp]
flattenRhs (LocWord n) = [yulInt n]
flattenRhs (LocBool b) = [yulBool b]
flattenRhs (LocStack i) = [YIdent (stkLoc i)]
flattenRhs (LocSeq ls) = concatMap flattenRhs ls
flattenRhs (LocEmpty size) = replicate size yulPoison
flattenRhs (LocNamed n) = [YIdent (yulVarName n)]

-- flattenRhs l = error ("flattenRhs: not implemented for "++show l)

flattenLhs :: Location -> [Name]
flattenLhs (LocStack i) = [stkLoc i]
flattenLhs (LocSeq ls) = concatMap flattenLhs ls
flattenLhs (LocNamed n) = [yulVarName n]
flattenLhs l = error ("flattenLhs: not implemented for " ++ show l)

genStmtWithComment :: Stmt -> TM [YulStmt]
genStmtWithComment (SComment c) = pure [YComment c]
genStmtWithComment s = do
  let comment = YComment (show s)
  body <- genStmt s
  pure (comment : body)

genStmt :: Stmt -> TM [YulStmt]
genStmt (SAssembly stmts) = do
  -- debug ["assembly:", render$ ppr (Yul stmts)]
  env <- getVarEnv
  pure (map (substAsmStmt env) stmts)
  where
    -- Substitute Hull variable references in assembly statements with their
    -- current Yul names.  Assembly blocks are emitted verbatim, so without
    -- this substitution a variable declared as `let msg_value : uint256` (which
    -- gets allocated as `_v0`) would be referenced by its Hull name and be
    -- undeclared in the generated Yul.
    substAsmStmt env (YAssign lhs e) =
      YAssign (map (substAsmName env) lhs) (substAsmExp env e)
    substAsmStmt env (YExp e) =
      YExp (substAsmExp env e)
    substAsmStmt env (YIf e body) =
      YIf (substAsmExp env e) (map (substAsmStmt env) body)
    substAsmStmt _ s = s

    substAsmExp env (YIdent n) = case Map.lookup (show n) env of
      Just loc -> case flattenRhs loc of
        [e'] -> e'
        _ -> YIdent n -- multi-slot locations cannot appear in assembly
      Nothing -> YIdent n
    substAsmExp env (YCall f args) = YCall f (map (substAsmExp env) args)
    substAsmExp _ e = e

    substAsmName env n = case Map.lookup (show n) env of
      Just (LocStack i) -> stkLoc i
      Just (LocNamed n') -> fromString n'
      _ -> n
genStmt (SAlloc name typ) = allocVar name typ
genStmt (SAssign name expr) = hullAssign name expr
genStmt (SReturn expr) = do
  debug [">SReturn: ", show expr]
  (stmts, loc) <- genExpr expr
  case loc of
    LocUnit -> pure (stmts ++ [YLeave])
    _ -> do
      resultLoc <- lookupVar "_result"
      let stmts' = copyLocs resultLoc loc
      pure (stmts ++ stmts' ++ [YLeave])
genStmt (SBlock stmts) = withLocalEnv do genStmts stmts
genStmt (SMatch _ e alts) = do
  (scrutStmts, scrutineeLoc) <- genExpr e
  -- debug ["> SMatch: ", show e , ":", show sty, " @ " , show scrutineeLoc]
  matchStmts <- case normalizeLoc scrutineeLoc of
    loc@(LocEmpty _) -> error ("SMatch: invalid location " ++ show loc)
    LocSeq (loctag : rest) -> genSwitch loctag (LocSeq rest) alts
    -- Special case: only tag, empty payload
    loctag -> genSwitch loctag LocUnit alts
  pure (scrutStmts ++ matchStmts)
  where
    genSwitch :: Location -> Location -> [Alt] -> TM [YulStmt]
    genSwitch tag payload altBranches = do
      (yulAlts, yulDefault) <- genNAlts payload altBranches
      pure [YSwitch (loadLoc tag) yulAlts yulDefault]
genStmt (SFunction name args ret stmts) = withLocalEnv do
  -- debug ["> SFunction: ", name, " ", show args, " -> ", show ret]
  yulArgs <- placeArgs args
  yreturns <- case stripTypeName ret of -- FIXME: temp hack for main
    TUnit
      | name == "main" -> YReturns <$> place "_result" TWord
      | otherwise -> pure YNoReturn
    TWord -> YReturns <$> placeResult
    _ -> do
      res <- place "_result" ret
      pure $
        if zeroSizedType ret
          then YNoReturn
          else YReturns res
  yulBody <- genStmts stmts
  -- debug ["< SFunction: ", name, " ", show yulArgs, " -> ", show yreturns]
  return [YFun (yulFunName name) yulArgs yreturns yulBody]
  where
    placeArgs :: [Arg] -> TM [Name]
    placeArgs as = concat <$> mapM placeArg as
    placeArg :: Arg -> TM [Name]
    placeArg (TArg argName typ) | isWordType typ = do
      let loc = LocNamed argName
      insertVar argName loc
      return [yulVarName argName]
    placeArg (TArg argName typ) = place argName typ
    placeResult :: TM [Name]
    placeResult = do
      let resultLoc = LocNamed "_result"
      insertVar "_result" resultLoc
      return ["_result"]
    place :: Hull.Name -> Type -> TM [Name]
    place varName typ = do
      loc <- buildLoc typ
      insertVar varName loc
      return (flattenLhs loc)
genStmt (SExpr e) = fst <$> genExpr e
genStmt (SRevert s) = pure (revertStmt s)
genStmt e = error $ "genStmt unimplemented for: " ++ show e

-- If the statement is a function definition, record its type
scanStmt :: Stmt -> TM ()
scanStmt (SFunction name args ret _) = do
  let argTypes = map (\(TArg _ t) -> t) args
  let info = FunInfo argTypes ret
  insertFun name info
scanStmt _ = pure ()

genBody :: Body -> TM [YulStmt]
genBody stmts = concat <$> mapM genStmt stmts

genBinAlts :: Location -> [Alt] -> TM [(YLiteral, [YulStmt])]
genBinAlts payload [Alt _ lname lbody, Alt _ rname rbody] = do
  yulLStmts <- withName lname payload lbody
  yulRStmts <- withName rname payload rbody
  pure [(YulFalse, yulLStmts), (YulTrue, yulRStmts)]
  where
    withName name loc body = withLocalEnv do
      insertVar name loc
      genBody body
genBinAlts _ alts =
  error
    ( "genAlts: invalid number of alternatives:\n"
        ++ unlines (map (render . ppr) alts)
    )

genNAlts :: Location -> [Alt] -> TM (YulCases, YulDefault)
genNAlts payload alts = do
  results <- mapM (genAlt payload) alts
  return (gather results)
  where
    gather = foldr combine ([], Nothing)
    combine (Left (tag, stmts)) (cases, def) = ((tag, stmts) : cases, def)
    combine (Right stmts) (cases, _) = (cases, Just stmts)

genAlt :: Location -> Alt -> TM (Either YulCase YulBlock)
genAlt payload (Alt (PCon con) name body) = withLocalEnv do
  insertVar name payload
  altStmts <- genBody body
  pure (Left (yulCon con, altStmts))
  where
    yulCon CInl = YulFalse
    yulCon CInr = YulTrue
    yulCon (CInK k) = YulNumber (fromIntegral k)
genAlt _ (Alt (PIntLit k) _ body) = withLocalEnv do
  altStmts <- genBody body
  pure (Left (YulNumber (fromIntegral k), altStmts))
genAlt payload (Alt (PVar name) _ body) = do
  insertVar name payload
  altStmts <- genBody body
  pure (Right altStmts)
genAlt _ alt = error ("genAlt unimplemented for: " ++ show alt)

allocVar :: Hull.Name -> Type -> TM [YulStmt]
allocVar name TWord = do
  insertVar name (LocNamed name)
  pure [YulAlloc (yulVarName name)]
allocVar name typ = do
  (stmts, loc) <- hullAlloc typ
  insertVar name loc
  return stmts

freshStackLoc :: TM Location
freshStackLoc = LocStack <$> freshId

buildLoc :: Type -> TM Location
buildLoc TWord = LocStack <$> freshId
buildLoc TBool = LocStack <$> freshId
buildLoc t@(TSum _ _) = LocSeq <$> sequence (replicate (sizeOf t) freshStackLoc)
buildLoc t@(TSumN _) = LocSeq <$> sequence (replicate (sizeOf t) freshStackLoc)
buildLoc TUnit = pure (LocSeq [])
buildLoc (TPair t1 t2) = LocSeq <$> sequence [buildLoc t1, buildLoc t2]
buildLoc (TNamed _ ty) = buildLoc ty
buildLoc t = error ("cannot build location for " ++ show t)

hullAlloc :: Type -> TM ([YulStmt], Location)
hullAlloc t = do
  loc <- buildLoc t
  let stmts = allocLoc loc
  pure (stmts, loc)

stackSlots :: Location -> [Int]
stackSlots (LocStack i) = [i]
stackSlots (LocSeq ls) = concatMap stackSlots ls
stackSlots _ = []

allocLoc :: Location -> [YulStmt]
allocLoc loc = [YulAlloc (stkLoc i) | i <- stackSlots loc]

allocWord :: TM ([YulStmt], Location)
allocWord = do
  n <- freshId
  let loc = LocStack n
  pure ([YulAlloc (stkLoc n)], loc)

hullAssign :: Expr -> Expr -> TM [YulStmt]
hullAssign lhs rhs = do
  (stmtsLhs, locLhs) <- genExpr lhs
  (stmtsRhs, locRhs) <- genExpr rhs
  if sizeOf locLhs == 0
    then pure stmtsRhs
    else pure (stmtsLhs ++ stmtsRhs ++ copyLocs locLhs locRhs)

loadLoc :: (HasCallStack) => Location -> YulExp
loadLoc (LocWord n) = YLit (YulNumber (fromIntegral n))
loadLoc (LocBool b) = YLit (if b then YulTrue else YulFalse)
loadLoc (LocStack i) = YIdent (stkLoc i)
loadLoc (LocNamed n) = YIdent (yulVarName n)
loadLoc (LocEmpty _) = yulPoison
loadLoc loc = error ("cannot loadLoc " ++ show loc)

-- copyLocs l r copies the value of r to l
copyLocs :: (HasCallStack) => Location -> Location -> [YulStmt]
copyLocs l r@(LocSeq _) = concat $ zipWith copyLocs (flattenLoc l) (flattenLoc r)
copyLocs l@(LocSeq _) r = concat $ zipWith copyLocs (flattenLoc l) (flattenLoc r)
copyLocs (LocStack _) (LocEmpty _) = []
copyLocs (LocStack i) r = [YAssign [stkLoc i] (loadLoc r)]
copyLocs (LocNamed n) r = [YAssign [yulVarName n] (loadLoc r)]
copyLocs l r = error $ "copy: type mismatch - LHS: " ++ show l ++ " RHS: " ++ show r

flattenLoc :: Location -> [Location]
flattenLoc (LocSeq ls) = concatMap flattenLoc ls
flattenLoc l = [l]

-- get rid of empty/nested sequences
normalizeLoc :: Location -> Location
normalizeLoc loc@(LocSeq _) = case flattenLoc loc of
  [l] -> l
  ls' -> LocSeq ls'
normalizeLoc loc = loc

genStmts :: [Stmt] -> TM [YulStmt]
genStmts stmts = do
  mapM_ scanStmt stmts -- scan for functions and record their types
  concat <$> mapM genStmt stmts

translateObject :: Object -> TM YulObject
translateObject (Object name code inners) = do
  yulCode <- translateStmts code
  yulInners <- mapM (fmap InnerObject . translateObject) inners
  pure (YulObject name (YulCode yulCode) yulInners)

translateStmts :: [Stmt] -> TM [YulStmt]
translateStmts stmts = do
  -- assuming the result goes into `_mainresult`
  let hasMain = any isMain stmts
  payload <- genStmts stmts
  let resultExp = YCall (yulFunName "main") []
  let epilog = if hasMain then [YLet ["_mainresult"] (Just resultExp)] else []
  return (payload ++ epilog)

isMain :: Stmt -> Bool
isMain (SFunction "main" _ _ _) = True
isMain _ = False

-- TODO: analyse main type
-- e.g. mainType :: Stmt -> Maybe Type

isFunction :: Stmt -> Bool
isFunction (SFunction {}) = True
isFunction _ = False

addMain :: [Stmt] -> [Stmt]
addMain stmts = functions ++ [SFunction "main" [] TWord other]
  where
    (functions, other) = span isFunction stmts

class HasSize a where
  sizeOf :: a -> Int

instance HasSize Type where
  sizeOf TWord = 1
  sizeOf TBool = 1
  sizeOf (TPair t1 t2) = sizeOf t1 + sizeOf t2
  sizeOf (TSum t1 t2) = 1 + max (sizeOf t1) (sizeOf t2)
  sizeOf (TSumN ts) = 1 + maximum (map sizeOf ts)
  sizeOf (TFun _ _) = error "sizeOf: function type has no runtime representation"
  sizeOf TUnit = 0
  sizeOf (TNamed _ t) = sizeOf t

instance HasSize Location where
  sizeOf (LocEmpty n) = n
  sizeOf (LocSeq ls) = sum (map sizeOf ls)
  sizeOf _ = 1

-- sizeOf A + paddingSize A B  == max (sizeOf A) (sizeOf B)
paddingSize :: (HasSize a, HasSize b) => a -> b -> Int
paddingSize t1 t2 = max 0 (sizeOf t2 - sizeOf t1)

-- sizeOf loc `paddedTo` B == max (sizeOf loc) (sizeOf B)
paddedTo :: Location -> Type -> Location
paddedTo loc ty = case paddingSize loc ty of
  0 -> loc
  n -> LocPair loc (LocEmpty n)

padToSize :: Location -> Int -> Location
padToSize loc n = case max 0 (n - sizeOf loc) of
  0 -> loc
  m -> LocPair loc (LocEmpty m)

-- | Check if a Hull type is word-sized (unwrapping TNamed wrappers and trivial sums)
isWordType :: Type -> Bool
isWordType TWord = True
isWordType (TNamed _ t) = isWordType t
isWordType (TSumN [t]) = isWordType t
isWordType _ = False

-- simulate LLVM "poison" value
yulPoison :: YulExp
yulPoison = YLit (YulNumber 911)

-- Cannot use $poison, because Yul is strict
-- yulPoison = YCall "$poison" []
