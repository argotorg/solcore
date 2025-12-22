{-|
Module      : Solcore.Desugarer.StringLiteral
Description : Desugars string literals to memory pointers

Every occurrence of a string literal expression is desugared to the application
of a function. This function stores each 32-byte chunk of the literal in memory
and returns a pointer to the total size in bytes, followed by the chunks.
-}
module Solcore.Desugarer.StringLiteral where

import Solcore.Frontend.Syntax
import Data.Generics ( mkM, everywhereM )
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.ByteString as BS (ByteString, length)
import Solcore.Primitives.Primitives (word)
import Language.Yul
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Solcore.Desugarer.ContractDispatch (generateStoreBytes)


data Env = Env {
             count      :: Int
           , strLitFuns :: [FunDef Name]
           }
type StrLitM a = State Env a

-- | Desugars all occurrences of string literals inside a compilation unit.
desugarStrLit :: CompUnit Name -> CompUnit Name
desugarStrLit c = c { contracts = evalState (desugarDecls $ contracts c) (Env 0 []) } where
    desugarDecls decls = do
        res    <- everywhereM (mkM desugarInExp) decls
        extras <- gets strLitFuns
        return $ (TFunDef <$> extras) ++ res

-- | Desugars all occurrences of string literals inside an expression.
desugarInExp :: Exp Name -> StrLitM (Exp Name)
desugarInExp (Lit (StrLit s)) = do
    idx <- gets count
    let strFun = literalToFun idx s
    modify (\env -> env { count = idx + 1, strLitFuns = strFun : strLitFuns env } )
    return $ call (sigName $ funSignature strFun) []
desugarInExp (TyExp exp tp) = do
    desugared <- desugarInExp exp
    return $ TyExp desugared tp
desugarInExp (Con c exps) = do
    desugared <- mapM desugarInExp exps
    return $ Con c desugared
desugarInExp (FieldAccess exp c) = do
    desugared <- mapM desugarInExp exp
    return $ FieldAccess desugared c
desugarInExp (Call exp f exps) = do
    desugaredExp  <- mapM desugarInExp exp
    desugaredExps <- mapM desugarInExp exps
    return $ Call desugaredExp f desugaredExps
desugarInExp (Lam params bdy tp) = do
    desugared <- everywhereM (mkM desugarInExp) bdy
    return $ Lam params desugared tp
desugarInExp (Cond e1 e2 e3) = do
    desugared1 <- desugarInExp e1
    desugared2 <- desugarInExp e2
    desugared3 <- desugarInExp e3
    return $ Cond desugared1 desugared2 desugared3
desugarInExp (Indexed e1 e2) = do
    desugared1 <- desugarInExp e1
    desugared2 <- desugarInExp e2
    return $ Indexed desugared1 desugared2
desugarInExp e = return e

-- | Defines a new function named strLit*idx*, which stores string chunks and
--   their total size in memory and returns the starting pointer.
literalToFun :: Int -> String -> FunDef Name
literalToFun idx s = FunDef {
    funSignature = Signature [] [] (Name $ "strLit" ++ show idx) [] (Just memoryString)
  , funDefBody   = (byteStringToBody . T.encodeUtf8 . T.pack) s
  }

-- | Chunks the bytestring and generates the code to store it in memory.
byteStringToBody :: BS.ByteString -> Body Name
byteStringToBody bs = decl ++ asm ++ ret where
    byteSize = toInteger $ BS.length bs
    wordSize = (byteSize + 31) `div` 32
    decl   = [
            declareAssignWord "size" (intLiteral byteSize),
            declareAssignWord "ptr"  (call "allocate_memory" [intLiteral $ (wordSize + 1) * 32]),
            declareWord "headPtr"
        ]
    asm    = [Asm $ assign ++ store]
    assign = [
            YExp $ YCall "mstore" [YIdent "ptr", YIdent "size"],
            YAssign ["headPtr"] (YCall "add" [YIdent "ptr", YLit $ YulNumber 32])
        ]
    store  = generateStoreBytes bs "headPtr"
    ret    = [
            Return $ Con "memory" [Var "ptr"]
        ]

--- Helpers ---

-- | Generate the memory(string) type
memoryString :: Ty
memoryString = TyCon (Name "memory") [TyCon (Name "string") []]

-- | Generate the literal expression corresponding to an integer
intLiteral :: Integer -> Exp Name
intLiteral i = Lit $ IntLit i

-- | Declare new variable of type word, with given name and no binding
declareWord :: Name -> Stmt Name
declareWord nm = Let nm (Just word) Nothing

-- | Declare new variable of type word, with given name and bound to expression
declareAssignWord :: Name -> Exp Name -> Stmt Name
declareAssignWord nm e = Let nm (Just word) (Just e)

-- | Call top level function with given name and arguments
call :: Name -> [Exp Name] -> Exp Name
call = Call Nothing
