module Solcore.Desugarer.ReplaceFunTypeArgs where 

import Control.Monad.State
import Data.Generics
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives


replaceFunParam :: Data a => a -> a 
replaceFunParam m 
  = evalState (everywhereM (mkM replaceFunParamM) m) 0

type ReplaceM a = State Int a 

replaceFunParamM :: Signature Name -> ReplaceM (Signature Name)
replaceFunParamM sig 
  = do 
      (args', pss, vss) <- unzip3 <$> mapM replace (sigParams sig)
      pure $ sig{ sigVars = sigVars sig ++ concat vss  
                , sigContext = concat pss ++ sigContext sig 
                , sigParams = args'
                }


freshTy :: ReplaceM (Ty, Tyvar) 
freshTy = do 
  i <- get 
  let
    v = TVar $ Name ("$v_" ++ show i)
    n = TyVar v   
  put (i + 1)
  pure (n, v) 

replace :: Param Name -> ReplaceM (Param Name, [Pred], [Tyvar])
replace (Untyped n) = pure (Untyped n, [], [])
replace (Typed n t@(_ :-> _))
  = do 
      (t1,v) <- freshTy 
      let (args,ret) = splitTy t 
          argTy = tupleTyFromList args 
          p = InCls invokableName t1 [argTy, ret]
      pure (Typed n t1, [p], [v])
replace (Typed n t) = pure (Typed n t, [], [])



