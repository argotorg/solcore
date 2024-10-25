{-# LANGUAGE OverloadedStrings #-}
module Translate where
import Data.List(nub, union)
import GHC.Stack
import Language.Core hiding(Name)
import qualified Language.Core as Core
import TM
import Language.Yul
import Solcore.Frontend.Syntax.Name
import Data.String

import Common.Monad
import Common.Pretty

genExpr :: Expr -> TM ([YulStmt], Location)
genExpr (EWord n) = pure ([], LocWord n)
genExpr (EBool b) = pure ([], LocBool b)
genExpr (EVar name) = do
    loc <- lookupVar name
    pure ([], loc)
genExpr (EPair e1 e2) = do
    (stmts1, loc1) <- genExpr e1
    (stmts2, loc2) <- genExpr e2
    pure (stmts1 ++ stmts2, LocSeq [loc1,loc2])
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
genExpr (EInl (TSum l r) e) = do
    (stmts, loc) <- genExpr e
    let loc' = loc `padToSize` sizeOf r
    pure (stmts, LocSeq[LocBool False,loc'])

genExpr (EInr (TSum l r) e) = do
    (stmts, loc) <- genExpr e
    let loc' = loc `paddedTo` r
    pure (stmts, LocSeq[LocBool True, loc'])
genExpr (EInl (TNamed n t) e) = genExpr (EInl t e)
genExpr (EInr (TNamed n t) e) = genExpr (EInr t e)

genExpr (EInK k (TSumN ts) e) = do
    (stmts, loc) <- genExpr e
    let maxsize = maximum (map sizeOf ts)
    let loc' = loc `padToSize` maxsize
    pure (stmts, LocSeq[LocWord (fromIntegral k), loc'])

genExpr EUnit = pure ([], LocUnit)
genExpr (ECall name args) = do
    (argCodes, argLocs) <- unzip <$> mapM genExpr args
    let argsCode = concat argCodes
    let yulArgs = concatMap flattenRhs argLocs
    funInfo <- lookupFun name
    (resultCode, resultLoc) <- coreAlloc (fun_result funInfo)
    let callExpr = YCall (yulFunName name) yulArgs
    let callCode = case resultLoc of  -- handle void functions
            LocUnit -> [YExp callExpr]
            _ -> [YAssign (flattenLhs resultLoc) callExpr]
    pure (argsCode++resultCode++callCode, resultLoc)
genExpr e = error ("genExpr: not implemented for "++show e)


yulFunName :: Core.Name -> Name
yulFunName = fromString . ("usr$" ++)

flattenRhs :: Location -> [YulExp]
flattenRhs (LocWord n) = [yulInt n]
flattenRhs (LocBool b) = [yulBool b]
flattenRhs (LocStack i) = [YIdent (stkLoc i)]
flattenRhs (LocSeq ls) = concatMap flattenRhs ls
flattenRhs (LocEmpty size) = replicate size yulPoison
flattenRhs l = error ("flattenRhs: not implemented for "++show l)

flattenLhs :: Location -> [Name]
flattenLhs (LocStack i) = [stkLoc i]
flattenLhs (LocSeq ls) = concatMap flattenLhs ls
flattenLhs l = error ("flattenLhs: not implemented for "++show l)

genStmtWithComment :: Stmt -> TM [YulStmt]
genStmtWithComment (SComment c) = pure [YComment c]
genStmtWithComment s = do
    let comment = YComment (show s)
    body <- genStmt s
    pure (comment : body)

genStmt :: Stmt -> TM [YulStmt]
genStmt (SAssembly stmts) = do
    debug ["assembly:", render$ ppr (Yul stmts)]
    pure stmts

genStmt (SAlloc name typ) = allocVar name typ
genStmt (SAssign name expr) = coreAssign name expr

genStmt (SReturn expr) = do
    (stmts, loc) <- genExpr expr
    case loc of
        LocUnit -> pure (stmts ++ [YLeave])
        _ -> do
            resultLoc <- lookupVar "_result"
            let stmts' = copyLocs resultLoc loc
            pure (stmts ++ stmts' ++ [YLeave])

genStmt (SBlock stmts) = withLocalEnv do genStmts stmts

genStmt (SMatch sty e alts) = do
    (stmts, scrutineeLoc) <- genExpr e
    -- debug ["> SMatch: ", show e , ":", show sty, " @ " , show scrutineeLoc]
    case normalizeLoc scrutineeLoc of
        loc@(LocEmpty n) -> error ("SMatch: invalid location " ++ show loc)
        LocSeq (loctag:rest) ->  genSwitch loctag (LocSeq rest) alts
        -- Special case: only tag, empty payload
        loctag -> genSwitch loctag LocUnit alts
     where
        genSwitch :: Location -> Location -> [Alt] -> TM [YulStmt]
        genSwitch tag payload alts = do
            yulAlts <- genNAlts payload alts
            pure [YSwitch (loadLoc tag) yulAlts Nothing]

genStmt (SFunction name args ret stmts) = withLocalEnv do
    debug ["> SFunction: ", name, " ", show args, " -> ", show ret]
    yulArgs <- placeArgs args
    yreturns <- case stripTypeName ret of -- FIXME: temp hack for main
        TUnit | name == "main" -> YReturns <$> place "_result" TWord
              | otherwise-> pure YNoReturn
        _  -> YReturns <$> place "_result" ret
    yulBody <- genStmts stmts
    debug ["< SFunction: ", name, " ", show yulArgs, " -> ", show yreturns]
    return [YFun (yulFunName name) yulArgs yreturns yulBody]
    where
        placeArgs :: [Arg] -> TM [Name]
        placeArgs as = concat <$> mapM placeArg as
        placeArg :: Arg -> TM [Name]
        placeArg (TArg name typ) = place name typ
        place :: Core.Name -> Type -> TM [Name]
        place name typ = do
            loc <- buildLoc typ
            insertVar name loc
            return (flattenLhs loc)

genStmt (SExpr e) = fst <$> genExpr e
genStmt (SRevert s) = pure
  [ YExp $ YCall "mstore" [yulInt 0, YLit (YulString s)]
  , YExp $ YCall "revert" [yulInt 0, yulInt (length s)]
  ]

genStmt (SExpr e) = do
    (stmts, loc) <- genExpr e
    pure stmts

genStmt e = error $ "genStmt unimplemented for: " ++ show e

-- If the statement is a function definition, record its type
scanStmt :: Stmt -> TM ()
scanStmt (SFunction name args ret stmts) = do
    let argTypes = map (\(TArg _ t) -> t) args
    let info = FunInfo argTypes ret
    insertFun name info
scanStmt _ = pure ()

genBinAlts :: Location -> [Alt] -> TM [(YLiteral, [YulStmt])]
genBinAlts payload [Alt lcon lname lstmt, Alt rcon rname rstmt] = do
    yulLStmts <- withName lname payload lstmt
    yulRStmts <- withName rname payload rstmt
    pure [(YulFalse, yulLStmts), (YulTrue, yulRStmts)]
    where
        withName name loc stmt = withLocalEnv do
            insertVar name loc
            genStmt stmt
genBinAlts _ alts = error("genAlts: invalid number of alternatives:\n" 
                          ++ unlines(map (render . ppr) alts) )

genNAlts :: Location -> [Alt] -> TM [(YLiteral, [YulStmt])]
genNAlts payload alts = do mapM (genAlt payload) alts

genAlt :: Location -> Alt -> TM (YLiteral, [YulStmt])
genAlt payload (Alt con name stmt) = withLocalEnv do
    insertVar name payload
    yulStmts <- genStmt stmt
    pure (yulCon con, yulStmts)
    where
        yulCon CInl = YulFalse
        yulCon CInr = YulTrue
        yulCon (CInK k) = YulNumber (fromIntegral k)

allocVar :: Core.Name -> Type -> TM [YulStmt]
allocVar name typ = do
    (stmts, loc) <- coreAlloc typ
    insertVar name loc
    return stmts

freshStackLoc :: TM Location
freshStackLoc = LocStack <$> freshId

buildLoc :: Type -> TM Location
buildLoc TWord = LocStack <$> freshId
buildLoc TBool = LocStack <$> freshId

buildLoc t@(TSum t1 t2) = LocSeq <$> sequence (replicate (sizeOf t) freshStackLoc)
buildLoc t@(TSumN ts) = LocSeq <$> sequence (replicate (sizeOf t) freshStackLoc)
buildLoc TUnit = pure (LocSeq [])
buildLoc (TPair t1 t2) = LocSeq <$> sequence [buildLoc t1, buildLoc t2]
buildLoc (TNamed n ty) = buildLoc ty

buildLoc t = error ("cannot build location for "++show t)

coreAlloc :: Type -> TM ([YulStmt], Location)
coreAlloc t = do
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


coreAssign :: Expr -> Expr -> TM [YulStmt]
coreAssign lhs rhs = do
    (stmts1, locLhs) <- genExpr lhs
    (stmts2, locRhs) <- genExpr rhs
    let stmts3 = copyLocs locLhs locRhs
    pure (stmts1 ++ stmts2 ++ stmts3)

loadLoc :: Location -> YulExp
loadLoc (LocWord n) = YLit (YulNumber (fromIntegral n))
loadLoc (LocBool b) = YLit (if b then YulTrue else YulFalse)
loadLoc (LocStack i) = YIdent (stkLoc i)
loadLoc (LocEmpty _) = yulPoison
loadLoc loc = error ("cannot loadLoc "++show loc)

-- copyLocs l r copies the value of r to l
copyLocs :: HasCallStack => Location -> Location -> [YulStmt]
copyLocs l r@(LocSeq rs) = concat $ zipWith copyLocs (flattenLoc l) (flattenLoc r)
copyLocs l@(LocSeq ls) r = concat $ zipWith copyLocs (flattenLoc l) (flattenLoc r)
copyLocs (LocStack i) (LocEmpty _) = []
copyLocs (LocStack i) r = [YAssign [stkLoc i] (loadLoc r)]


copyLocs l r = error $ "copy: type mismatch - LHS: " ++ show l ++ " RHS: " ++ show r

flattenLoc :: Location -> [Location]
flattenLoc (LocSeq ls) = concatMap flattenLoc ls
flattenLoc l = [l]

-- get rid of empty/nested sequences
normalizeLoc :: Location -> Location
normalizeLoc loc@(LocSeq ls) = case flattenLoc loc of
    [l] -> l
    ls' -> LocSeq ls'
normalizeLoc loc = loc

genStmts :: [Stmt] -> TM [YulStmt]
genStmts stmts = do
    mapM_ scanStmt stmts   -- scan for functions and record their types
    concat <$> mapM genStmtWithComment stmts

translateCore :: Core -> TM Yul
translateCore (Core stmts) = translateStmts stmts

translateStmts :: [Stmt] -> TM Yul
translateStmts stmts = do
    -- assuming the result goes into `_wrapresult`
    let hasMain = any isMain stmts
    if hasMain
      then writeln "found main"
      else writeln "no main found, adding one"
    let stmts' = if hasMain then stmts else addMain stmts
    payload <- genStmts stmts'
    let resultExp = YCall (yulFunName "main") []
    let epilog = [YAssign1 "_wrapresult" resultExp]
    return $ Yul ( payload ++ epilog )


isMain :: Stmt -> Bool
isMain (SFunction "main" _ _ _) = True
isMain _ = False
-- TODO: analyse main type
-- e.g. mainType :: Stmt -> Maybe Type

isFunction (SFunction {}) = True
isFunction _ = False

addMain :: [Stmt] -> [Stmt]
addMain stmts = functions ++ [SFunction "main" [] TWord other]
  where (functions, other) = span isFunction stmts

class HasSize a where
    sizeOf :: a -> Int

instance HasSize Type where
    sizeOf TWord = 1
    sizeOf TBool = 1
    sizeOf (TPair t1 t2) = sizeOf t1 + sizeOf t2
    sizeOf (TSum t1 t2)  = 1 + max (sizeOf t1) (sizeOf t2)
    sizeOf (TSumN ts) = 1 + maximum (map sizeOf ts)
    sizeOf TUnit = 0
    sizeOf (TNamed _ t) = sizeOf t

instance HasSize Location where
    sizeOf (LocEmpty n)  = n
    sizeOf (LocSeq ls)   = sum (map sizeOf ls)
    sizeOf l = 1

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

-- simulate LLVM "poison" value
yulPoison :: YulExp
yulPoison = YLit (YulNumber 911)
-- Cannot use $poison, because Yul is strict
-- yulPoison = YCall "$poison" []
