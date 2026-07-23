{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Assemble a translated Yul object into final Yul text, and drive the whole
-- hull -> Yul step in-process. This holds the wrapping logic that used to live
-- in @yule/Main.hs@, so both the @yule@ CLI and the in-memory API share it.
module Language.Hull.ToYul.Assemble
  ( defaultYuleOptions,
    objectToYul,
    wrapInObject,
  )
where

import Common.Pretty
import Control.Exception (SomeException, evaluate, try)
import Language.Hull (Object)
import Language.Hull.TcEnv (emptyHullTcEnv)
import Language.Hull.TcMonad (runHullTcM)
import Language.Hull.ToYul.Options (Options (..))
import Language.Hull.ToYul.TM (runTM)
import Language.Hull.ToYul.Translate (translateObject)
import Language.Hull.TypeCheck (checkObject)
import Language.Yul
import Language.Yul.QuasiQuote

-- | Options for driving the Yul backend in-process, matching the @yule@ CLI
-- defaults (deployment code on, type checking on).
defaultYuleOptions :: Options
defaultYuleOptions =
  Options
    { input = "",
      contract = "Output",
      output = "Output.sol",
      verbose = False,
      debug = False,
      compress = False,
      wrap = False,
      runOnce = False,
      noTypeCheck = False
    }

-- | Type-check, translate and render one hull object to Yul (with deployment
-- code). Returns @Left@ with a diagnostic on a Hull/Yul type error or an
-- unimplemented translation case.
objectToYul :: Object -> IO (Either String String)
objectToYul obj = do
  tcResult <- runHullTcM (checkObject obj) emptyHullTcEnv
  case tcResult of
    Left err -> pure (Left err)
    Right () -> do
      -- The translator uses `error` for unimplemented cases; force the rendered
      -- output inside `try` so those surface as diagnostics rather than crashes.
      outcome <-
        try $ do
          yulPreobject <- runTM defaultYuleOptions (translateObject obj)
          let rendered = render (wrapInObject True yulPreobject)
          _ <- evaluate (length rendered)
          pure rendered
      pure $ case (outcome :: Either SomeException String) of
        Left ex -> Left ("Yul translation error:\n" ++ show ex)
        Right rendered -> Right rendered

-- wrap in a Yul object with the given name
wrapInObject :: Bool -> YulObject -> Doc
wrapInObject deploy yulo@(YulObject name code inners)
  | deploy = ppr (createDeployment yulo)
  | otherwise = ppr (YulObject name (addMemInit (addRetCode code)) inners)

addMemInit :: YulCode -> YulCode
addMemInit c = YulCode [[yulStmt| mstore(64, memoryguard(128)) |]] <> c

addRetCode :: YulCode -> YulCode
addRetCode c = c <> retCode
  where
    retCode =
      YulCode
        [yulBlock|
    {
      mstore(0, _mainresult)
      return(0, 32)
    }
    |]

deployCode :: String -> Bool -> YulCode
deployCode _name withStart = YulCode $ go withStart
  where
    go True = [[yulStmt| usr$_start() |]]
    go False = []

createDeployment :: YulObject -> YulObject
createDeployment (YulObject yulName yulCode [InnerObject (YulObject innerName innerCode [])]) =
  YulObject yulName yulCode' [yulInner']
  where
    yulCode' = yulCode <> deployCode innerName True
    yulInner' = InnerObject (YulObject innerName (addRetCode innerCode) [])
createDeployment (YulObject yulName yulCode []) =
  YulObject yulName' yulCode' [yulInner']
  where
    yulName' = yulName <> "Deploy"
    yulCode' = deployCode yulName False
    yulInner' = InnerObject (YulObject yulName (addRetCode yulCode) [])
createDeployment _ = error ("createDeployment not implemented for this type of object")
