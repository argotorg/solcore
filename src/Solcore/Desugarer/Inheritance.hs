--  Desugaring of contract-inheritance surface syntax (interface,
-- implements, inherits, super) into the class/instance/ADT representation.
--
-- interface I { ... } is already turned into a type class at parse time (each
-- method gains a self : self receiver, see Parser.Decl.interfaceP). This pass
-- handles the implements, inherits and super constructs:
--
--   * contract C implements I keeps C's storage-bearing (so the
--     field-access pass emits its selectors and the dispatcher finds its public
--     methods) and, in addition, emits an instance ContractStorage(CCxt) : I
--     whose method bodies are C's own method bodies, rewritten so field access
--     goes through the reified self receiver (reusing the field-access engine).
--
--   * contract D inherits B1, B2, ... is lowered by C3-linearizing the bases
--     (classic Solidity's method-resolution order) and flattening that order into
--     the leaf contract: D's storage becomes every ancestor's fields (base first,
--     a diamond's shared base included once) followed by D's own, and D's methods
--     become the inherited methods with more-derived definitions overriding by
--     name. Each abstract base (a contract that some other contract inherits) is
--     dropped from the output: only leaf contracts are emitted. Interface
--     instances are generated for the union of every interface implemented
--     anywhere in the chain. Single inheritance is the special case (C3 of a
--     linear chain = the chain).
--
--   * super.m(args) in an overriding method is rewritten to a call to a
--     generated free function super$m$<p>(self, args) whose body is the next
--     implementation of m in the linearization, with its own field access routed
--     through the self parameter. Chained super threads through the following MRO
--     level, so an arbitrarily deep hierarchy is handled.
--
--   * base constructors are chained in C3 base-first order (Solidity's
--     construction order); `constructor(...) : Base(args)` initializer lists
--     forward arguments, resolved most-derived-first (see chainedConstructor).
--
-- Runs after name resolution and before the field-access pass, so method bodies
-- already distinguish fields from locals/params (Var), which is what the body
-- rewrite relies on. Because fields are re-included by name and the field-access
-- pass keys selectors on the field name in the leaf's flat layout, inherited
-- bodies read/write the correct storage slots without any selector rewriting.

module Solcore.Desugarer.Inheritance (inheritanceDesugarTopDecls) where

import Data.Generics (everywhere, listify, mkT)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Desugarer.FieldAccess (rewriteBodyWithSelf)
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty

inheritanceDesugarTopDecls :: [TopDecl Name] -> [TopDecl Name]
inheritanceDesugarTopDecls topdecls =
  concatMap (keepTopDecl contractMap baseNames) topdecls
    ++ concatMap (implementsInstances classEnv . fst) leafResults
    ++ concatMap snd leafResults
  where
    contractMap = contractsOf topdecls
    baseNames = baseNamesOf topdecls
    classEnv = classesOf topdecls
    leafResults =
      [ flattenWithSuper contractMap c
      | TContr c <- topdecls,
        not (isBase baseNames c)
      ]

-- Module-level views of the whole compilation unit

contractsOf :: [TopDecl Name] -> Map Name (Contract Name)
contractsOf tds = Map.fromList [(name c, c) | TContr c <- tds]

baseNamesOf :: [TopDecl Name] -> Set Name
baseNamesOf tds = Set.fromList (concat [contractInherits c | TContr c <- tds])

classesOf :: [TopDecl Name] -> Map Name (Class Name)
classesOf tds = Map.fromList [(className c, c) | TClassDef c <- tds]

isBase :: Set Name -> Contract Name -> Bool
isBase baseNames c = name c `Set.member` baseNames

keepTopDecl :: Map Name (Contract Name) -> Set Name -> TopDecl Name -> [TopDecl Name]
keepTopDecl contractMap baseNames (TContr c)
  | isBase baseNames c = []
  | otherwise = [TContr (fst (flattenWithSuper contractMap c))]
keepTopDecl _ _ d = [d]

-- The C3 linearization of a contract's bases, base-first (leaf last), as the
-- contracts present in this module. Classic Solidity gives the rightmost base
-- the highest precedence, i.e. standard C3 with the direct-base list processed
-- in reverse; `mro` returns the most-derived-first order and we reverse it so
-- the existing index-based `super` machinery (lower index = more base)
-- reproduces Solidity's resolution. Single inheritance is the special case
-- (C3 of a linear chain = the chain).
chainOf :: Map Name (Contract Name) -> Contract Name -> [Contract Name]
chainOf contractMap c =
  [c' | n <- reverse (mro contractMap (name c)), Just c' <- [Map.lookup n contractMap]]

-- C3 linearization (most-derived first). An absent (cross-module) base still
-- takes a position in the order but is filtered out when mapped back to a
-- Contract, so its fields/methods are simply not flattened in (same deferral as
-- the single-parent design).
mro :: Map Name (Contract Name) -> Name -> [Name]
mro contractMap n = n : c3merge (map (mro contractMap) revBases ++ [revBases])
  where
    revBases = maybe [] (reverse . contractInherits) (Map.lookup n contractMap)

-- Standard C3 merge: repeatedly take the head of the first list that does not
-- appear in the tail of any list, remove it everywhere, and continue. A
-- hierarchy with no valid linearization is a compile error.
c3merge :: [[Name]] -> [Name]
c3merge lists = case filter (not . null) lists of
  [] -> []
  nonEmpty ->
    case [h | (h : _) <- nonEmpty, all ((h `notElem`) . drop 1) nonEmpty] of
      (cand : _) -> cand : c3merge (map (filter (/= cand)) nonEmpty)
      [] -> error ("inheritance: cannot C3-linearize an inconsistent hierarchy: " ++ show lists)

-- Flattening an inheritance chain

-- The per-chain context the flattening and `super` helpers operate over: the
-- leaf contract's name, its flattened (root-first) field list, and the methods
-- defined at each chain level (root first).
data ChainCtx = ChainCtx
  { ccName :: Name,
    ccFields :: [Field Name],
    ccLevels :: [Map Name (FunDef Name)]
  }

ccArity :: ChainCtx -> Int
ccArity = length . ccLevels

ccMethodsAt :: ChainCtx -> Int -> Map Name (FunDef Name)
ccMethodsAt ctx i = ccLevels ctx !! i

definesAt :: ChainCtx -> Int -> Name -> Bool
definesAt ctx i g = Map.member g (ccMethodsAt ctx i)

nearestBelow :: ChainCtx -> Name -> Int -> Maybe Int
nearestBelow ctx g lvl = find (\j -> definesAt ctx j g) [lvl - 1, lvl - 2 .. 0]

topLevelOf :: ChainCtx -> Name -> Int
topLevelOf ctx g = case filter (\j -> definesAt ctx j g) [ccArity ctx - 1, ccArity ctx - 2 .. 0] of
  (j : _) -> j
  [] -> ccArity ctx - 1

flattenWithSuper :: Map Name (Contract Name) -> Contract Name -> (Contract Name, [TopDecl Name])
flattenWithSuper contractMap c
  | [_] <- chain = (c, [])
  | otherwise = (flatContract, superDecls)
  where
    chain = chainOf contractMap c
    ctx =
      ChainCtx
        { ccName = name c,
          ccFields = concatMap (contractFields . decls) chain,
          ccLevels = map (contractMethods . decls) chain
        }

    methodNames = nubOrder [sigName (funSignature fd) | a <- chain, CFunDecl fd <- decls a]
    leafFunDefs =
      [ fd {funDefBody = rewriteSupersAt ctx lvl (funDefBody fd)}
      | g <- methodNames,
        let lvl = topLevelOf ctx g,
        let fd = ccMethodsAt ctx lvl Map.! g
      ]

    dataDecls = [d | a <- chain, d@(CDataDecl _) <- decls a]
    -- Flat storage = every ancestor's fields (root first) then the leaf's. A
    -- diamond's shared base appears once in `chain`, so its fields are included
    -- once. Field names must be unique across the hierarchy (Solidity's rule),
    -- since selectors key on the name in the flat layout.
    fieldDecls
      | null dupFieldNames = [d | a <- chain, d@(CFieldDecl _) <- decls a]
      | otherwise =
          error
            ( "inheritance: duplicate field name(s) across the inheritance hierarchy of "
                ++ show (name c)
                ++ ": "
                ++ show dupFieldNames
            )
    dupFieldNames =
      [ n
      | (n, cnt) <- Map.toList (Map.fromListWith (+) [(fieldName f, 1 :: Int) | a <- chain, CFieldDecl f <- decls a]),
        cnt > 1
      ]
    ctorDecls = chainedConstructor chain
    flatContract =
      c
        { contractInherits = [],
          contractImplements = nubOrder (concatMap contractImplements chain),
          decls = dataDecls ++ fieldDecls ++ ctorDecls ++ map CFunDecl leafFunDefs
        }

    seedTargets =
      concat
        [ superTargetsAt ctx lvl (funDefBody (ccMethodsAt ctx lvl Map.! g))
        | g <- methodNames,
          let lvl = topLevelOf ctx g
        ]
    neededTargets = closeTargets ctx Set.empty seedTargets
    superDecls = [TFunDef (superFunFor ctx g p) | (g, p) <- Set.toList neededTargets]

-- constructor chaining

-- Emit the leaf's deployed constructor as the linearized base-constructor chain:
-- each ancestor's constructor body runs once, in C3 base-first order, then the
-- leaf's own body (Solidity's most-base-first construction order). Base-constructor
-- arguments come from the initializer lists (`constructor(...) : Base(args)`),
-- resolved most-derived-first and inlined by substituting the providing
-- constructor's parameters, so nested forwarding (`B(x) : A(x+1)`) bottoms out in
-- leaf-scope expressions. Each body is wrapped in a block to keep its locals
-- isolated. Field access in the inlined bodies uses the ordinary
-- contract-constructor path (the storage constant), so no `self` parameter is
-- needed here.
chainedConstructor :: [Contract Name] -> [ContractDecl Name]
chainedConstructor chain
  | not (any hasCtor chain) = []
  | otherwise = [CConstrDecl (Constructor leafParams body leafPayable [])]
  where
    leaf = last chain
    ctorOf nm = listToMaybe [ct | c <- chain, name c == nm, CConstrDecl ct <- decls c]
    hasCtor c = case ctorOf (name c) of Just _ -> True; Nothing -> False
    leafCtor = ctorOf (name leaf)
    leafParams = maybe [] constrParams leafCtor
    leafPayable = maybe False constrPayable leafCtor

    -- Per-contract parameter substitution (its params → leaf-scope arg
    -- expressions), built by walking the MRO most-derived first so a provider is
    -- resolved before its bases; the most-derived provider wins for a shared
    -- (diamond) base.
    sigma :: Map Name (Map Name (Exp Name))
    sigma = foldl visit Map.empty (reverse chain)
    visit sig d = case ctorOf (name d) of
      Nothing -> sig
      Just ct -> foldl (provide (Map.findWithDefault Map.empty (name d) sig)) sig (constrInits ct)
    provide sigD sig (baseNm, argExprs)
      | baseNm `Map.member` sig = sig
      | otherwise = case ctorOf baseNm of
          Nothing ->
            error ("inheritance: base `" ++ show baseNm ++ "` has no constructor to receive arguments")
          Just baseCt ->
            let baseParams = map paramName (constrParams baseCt)
                resolved = map (substVarsE sigD) argExprs
             in if length baseParams /= length resolved
                  then error ("inheritance: wrong number of arguments to base constructor `" ++ show baseNm ++ "`")
                  else Map.insert baseNm (Map.fromList (zip baseParams resolved)) sig

    body =
      [ Block (substVars (Map.findWithDefault Map.empty (name x) sigma) (constrBody ct))
      | x <- chain,
        Just ct <- [ctorOf (name x)],
        checkArgs x ct
      ]
    checkArgs x ct
      | null (constrParams ct) = True
      | name x == name leaf = True -- the leaf's params are the emitted constructor's own
      | Map.member (name x) sigma = True
      | otherwise =
          error
            ( "inheritance: base constructor `"
                ++ show (name x)
                ++ "` requires arguments; add `: "
                ++ show (name x)
                ++ "(...)` to a derived constructor"
            )

-- Replace every `Var n` (a constructor parameter reference) by its provided
-- expression. Used to inline base-constructor arguments.
substVars :: Map Name (Exp Name) -> Body Name -> Body Name
substVars sub body
  | Map.null sub = body
  | otherwise = everywhere (mkT f) body
  where
    f e@(Var n) = Map.findWithDefault e n sub
    f e = e

substVarsE :: Map Name (Exp Name) -> Exp Name -> Exp Name
substVarsE sub e
  | Map.null sub = e
  | otherwise = everywhere (mkT f) e
  where
    f x@(Var n) = Map.findWithDefault x n sub
    f x = x

-- super lowering

-- Rewrite every super.g(args) in a body known to sit at level lvl to a call
-- to the generated parent-level function, passing the reified self receiver.
-- An unresolved super (no ancestor defines g) is left untouched (a user error
-- that later type checking reports).
rewriteSupersAt :: ChainCtx -> Int -> Body Name -> Body Name
rewriteSupersAt ctx lvl = everywhere (mkT rw)
  where
    rw :: Exp Name -> Exp Name
    rw e@(Call (Just recv) g args)
      | isSuperReceiver recv =
          case nearestBelow ctx g lvl of
            Just p -> Call Nothing (superFnName g p) (selfExp : args)
            Nothing -> e
    rw e = e

superTargetsAt :: ChainCtx -> Int -> Body Name -> [(Name, Int)]
superTargetsAt ctx lvl body =
  [ (g, p)
  | Call (Just recv) g _ <- listify isSuperCall body,
    isSuperReceiver recv,
    Just p <- [nearestBelow ctx g lvl]
  ]

closeTargets :: ChainCtx -> Set (Name, Int) -> [(Name, Int)] -> Set (Name, Int)
closeTargets _ acc [] = acc
closeTargets ctx acc (t@(g, p) : rest)
  | t `Set.member` acc = closeTargets ctx acc rest
  | otherwise =
      let body = funDefBody (ccMethodsAt ctx p Map.! g)
          more = superTargetsAt ctx p body
       in closeTargets ctx (Set.insert t acc) (more ++ rest)

superFunFor :: ChainCtx -> Name -> Int -> FunDef Name
superFunFor ctx g p = FunDef False sig' body'
  where
    fd = ccMethodsAt ctx p Map.! g
    sig = funSignature fd
    sig' =
      sig
        { sigName = superFnName g p,
          sigParams = Typed False selfName (storageTy (ccName ctx)) : sigParams sig
        }
    body' = rewriteBodyWithSelf (ccName ctx) (ccFields ctx) selfName (rewriteSupersAt ctx p (funDefBody fd))

-- implements instances

implementsInstances :: Map Name (Class Name) -> Contract Name -> [TopDecl Name]
implementsInstances classEnv c =
  [ TInstDef (buildInstance classEnv (name c) (contractFields (decls c)) (contractMethods (decls c)) i)
  | i <- contractImplements c,
    Map.member i classEnv
  ]

buildInstance :: Map Name (Class Name) -> Name -> [Field Name] -> Map Name (FunDef Name) -> Name -> Instance Name
buildInstance classEnv cname fields methods iname =
  Instance
    { instDefault = False,
      instVars = [],
      instContext = [],
      instName = iname,
      paramsTy = [],
      mainTy = storageTy cname,
      instFunctions =
        [ buildMethod cname fields cfd
        | sig <- classSigs classEnv iname,
          Just cfd <- [Map.lookup (sigName sig) methods]
        ]
    }

classSigs :: Map Name (Class Name) -> Name -> [Signature Name]
classSigs classEnv iname = maybe [] signatures (Map.lookup iname classEnv)

buildMethod :: Name -> [Field Name] -> FunDef Name -> FunDef Name
buildMethod cname fields (FunDef _ sig body) =
  FunDef False sig' body'
  where
    sig' = sig {sigParams = Typed False selfName (storageTy cname) : sigParams sig}
    body' = rewriteBodyWithSelf cname fields selfName body

selfExp :: Exp Name
selfExp = FieldAccess Nothing selfName

selfName :: Name
selfName = Name "self"

isSuperReceiver :: Exp Name -> Bool
isSuperReceiver (Var (Name "super")) = True
isSuperReceiver _ = False

isSuperCall :: Exp Name -> Bool
isSuperCall (Call (Just recv) _ _) = isSuperReceiver recv
isSuperCall _ = False

superFnName :: Name -> Int -> Name
superFnName g p = Name ("super$" ++ simpleStr g ++ "$" ++ show p)

simpleStr :: Name -> String
simpleStr (Name s) = s
simpleStr (QualName _ s) = s

storageTy :: Name -> Ty
storageTy cname = TyCon (Name "ContractStorage") [TyCon (cxtName cname) []]

cxtName :: Name -> Name
cxtName (Name s) = Name (s ++ "Cxt")
cxtName (QualName n s) = QualName n (s ++ "Cxt")

contractFields :: [ContractDecl Name] -> [Field Name]
contractFields cds = [f | CFieldDecl f <- cds]

contractMethods :: [ContractDecl Name] -> Map Name (FunDef Name)
contractMethods cds = Map.fromList [(sigName (funSignature fd), fd) | CFunDecl fd <- cds]

nubOrder :: (Ord a) => [a] -> [a]
nubOrder = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
