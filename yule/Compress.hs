module Compress where
import Language.Core

class Compress a where
    compress :: a -> a

instance Compress Type where
    compress (TNamed n t@(TSum _ _)) = foldSum t
    compress t = t

foldSum :: Type -> Type
foldSum t = TSumN (go t) where
    go :: Type -> [Type]
    go (TSum t1 t2) = go t1 ++ go t2
    go t = [t]

-- foldIns :: Type -> Expr -> Expr
-- treat expressions of the form:
--  - inl(inr*(e))  e.g. inl<T0+T1+T2>(inr(e)) becomes in(1)<T0+T1+T2>(e)
--                       inl<T0+T1+T2>(e) becomes in(0)<T0+T1+T2>(e)
--  - inr+(e)       e.g. inr<T0+T1+T2>(inr(e)) becomes in(2)<T0+T1+T2>(e)
-- Note: for complex types, such as Option{(unit + Option{(unit + word)})}
--       inr(inr(x)) becomes in1(in1(x)) rather than in2(x)
compressInjections ty@(TSumN ts) e = go 0 e where
    arity = length ts
    go k e | k == arity-1 = EInK k ty (compress e)
    go k (EInr _ e) = go (k+1) e
    go k (EInl _ e) = EInK k ty e
    -- go k e = EInK k ty (compress e)

{- Compress match statements
    match e<sum(t1, t2)> with {
        inl(x) => s1
        inr(y) => s2 }
    becomes
    match e with {
        in1(x) => s1
        in2(x) => s2 }
    even if s2 is a match statement

    match e<sum(t1, t2, t3)> with {
        inl(x) => s1
        inr(y) => match y with {
        }
    }

    To do this we need to know the scrutinee type
-}
compressMatch cty@(TSumN ts) top@(SMatch ty e alts) = SMatch ty' e' (go 0 top) where
    ty' = compress ty
    e' = compress e
    arity = length ts
    go k s@(SMatch t@(TNamed n ty) e alts) = go k (SMatch ty e alts)
    go k s@(SMatch t@(TSum lty rty) e [Alt CInl ln left, Alt CInr rn right])
       -- last two alternatives in the chain
       | k == arity-2 = [Alt (CInK k )ln left', Alt (CInK (k+1)) rn right']
       -- not reached the end of the chain yet
       | otherwise = firstAlt:rest
       where
        left' = compress left
        right' = compress right
        firstAlt = Alt (CInK k) ln left'
        rest = go (k+1) right
    go k (SBlock [s]) = go k s
    go k s = error $ concat["compressMatch unimplemented for k=",show k," stmt: ", show s]

instance Compress Contract where
    compress c = c { ccStmts = map compress (ccStmts c) }

instance Compress a => Compress [a] where
    compress = map compress

instance Compress Stmt where
    compress (SFunction n args t stmts) = SFunction n 
                                            (compress args)
                                            (compress t) 
                                            (map compress stmts)
    compress (SReturn e) = SReturn (compress e)
    compress (SMatch t e alts) = compressMatch (compress t) (SMatch t e alts)
    compress s = s

instance Compress Arg where
    compress (TArg n t) = TArg n (compress t)

instance Compress Expr where
    compress e@(EInl ty _) = compressInjections (compress ty) e
    compress e@(EInr ty _) = compressInjections (compress ty) e
    compress (ECall n es) = ECall n (compress es)
    compress e = e