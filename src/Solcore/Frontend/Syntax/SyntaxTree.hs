{-# LANGUAGE PatternSynonyms #-}

module Solcore.Frontend.Syntax.SyntaxTree where

import Data.Generics (Data, Typeable)
import Data.List (union)
import Data.List.NonEmpty
import Language.Yul
import Prelude hiding (exp)
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name

-- compilation unit

data CompUnit
  = CompUnit
  { imports :: [Import],
    contracts :: [TopDecl]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data TopDecl
  = TContr Contract
  | TFunDef FunDef
  | TClassDef Class
  | TInstDef Instance
  | TDataDef DataTy
  | TSym TySym
  | TExportDecl Export
  | TPragmaDecl Pragma
  deriving (Eq, Ord, Show, Data, Typeable)

-- empty list in pragma: restriction on all class / instances

data PragmaType
  = NoCoverageCondition
  | NoPattersonCondition
  | NoBoundVariableCondition
  deriving (Eq, Ord, Show, Data, Typeable)

data PragmaStatus
  = Enabled
  | DisableAll
  | DisableFor (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)

data Pragma
  = Pragma
  { pragmaType :: PragmaType,
    pragmaStatus :: PragmaStatus
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data ModulePath
  = RelativePath Name
  | LibraryPath Name
  | ExternalPath Name Name
  deriving (Eq, Ord, Show, Data, Typeable)

data Export
  = ExportList [ExportSpec]
  | ExportModule ModulePath
  | ExportModuleAs ModulePath Name
  | ExportItemsFrom ModulePath ExportSelector
  deriving (Eq, Ord, Show, Data, Typeable)

data ConstructorSelector
  = SelectConstructors [Name]
  | SelectAllConstructors
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSpec
  = ExportName Name
  | ExportNameWithConstructors Name ConstructorSelector
  | ExportAll
  | ExportModuleAll ModulePath
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSelector
  = SelectExportItems [ExportSelectorEntry]
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSelectorEntry
  = SelectExportAllItems
  | SelectExportItem Name
  | SelectExportConstructors Name ConstructorSelector
  deriving (Eq, Ord, Show, Data, Typeable)

data Import
  = ImportModule {importModule :: ModulePath}
  | ImportAlias {importModule :: ModulePath, importAlias :: Name}
  | ImportOnly {importModule :: ModulePath, importItems :: ItemSelector}
  deriving (Eq, Ord, Show, Data, Typeable)

data ItemSelector
  = SelectItems [ItemSelectorEntry] [Name]
  deriving (Eq, Ord, Show, Data, Typeable)

data ItemSelectorEntry
  = SelectAllItems
  | SelectItem Name
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of the contract structure

data Contract
  = Contract
  { name :: Name,
    tyParams :: [Ty],
    decls :: [ContractDecl]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of a algebraic data type

data DataTy
  = DataTy
  { dataName :: Name,
    dataParams :: [Ty],
    dataConstrs :: [Constr]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Constr
  = Constr
  { constrName :: Name,
    constrTy :: [Ty]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- type definition

data Ty
  = TyConWithLocation NodeLocation Name [Ty] -- type constructor
  deriving (Eq, Ord, Show, Data, Typeable)

pattern TyCon :: Name -> [Ty] -> Ty
pattern TyCon n ts <- TyConWithLocation _ n ts
  where
    TyCon n ts = TyConWithLocation unlocatedNode n ts

{-# COMPLETE TyCon #-}

pattern (:->) :: Ty -> Ty -> Ty
pattern (:->) t1 t2 = TyCon (Name "->") [t1, t2]

tyName :: Ty -> Name
tyName (TyCon n _) = n

locatedTy :: SourceSpan -> Ty -> Ty
locatedTy sourceSpan (TyCon n ts) =
  TyConWithLocation (locatedNode sourceSpan) n ts

instance HasSourceSpan Ty where
  sourceSpanOf (TyConWithLocation location n ts) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ts]

data Pred = InCls
  { predName :: Name,
    predMain :: Ty,
    predParams :: [Ty]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HasSourceSpan Pred where
  sourceSpanOf (InCls n t ts) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf t, sourceSpanOf ts]

tysFrom :: [Pred] -> [Ty]
tysFrom = foldr go []
  where
    go p ac = (predMain p) : predParams p `union` ac

-- definition of type synonym

data TySym
  = TySym
  { symName :: Name,
    symVars :: [Ty],
    symType :: Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract constructor

data Constructor
  = Constructor
  { constrParams :: [Param],
    constrBody :: Body
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of classes and instances

data Class
  = Class
  { classboundvars :: [Ty],
    classContext :: [Pred],
    className :: Name,
    paramsVar :: [Ty],
    mainVar :: Ty,
    signatures :: [Signature]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Signature
  = Signature
  { sigVars :: [Ty],
    sigContext :: [Pred],
    sigName :: Name,
    sigParams :: [Param],
    sigReturn :: Maybe Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Instance
  = Instance
  { instDefault :: Bool,
    instVars :: [Ty],
    instContext :: [Pred],
    instName :: Name,
    paramsTy :: [Ty],
    mainTy :: Ty,
    instFunctions :: [FunDef]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract field variables

data Field
  = Field
  { fieldName :: Name,
    fieldTy :: Ty,
    fieldInit :: Maybe Exp
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of functions

data FunDef
  = FunDef
  { funSignature :: Signature,
    funDefBody :: Body
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data ContractDecl
  = CDataDecl DataTy
  | CFieldDecl Field
  | CFunDecl FunDef
  | CConstrDecl Constructor
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of statements

type Equation = ([Pat], [Stmt])

type Equations = [Equation]

data Stmt
  = AssignWithLocation NodeLocation Exp Exp -- assignment
  | StmtPlusEqWithLocation NodeLocation Exp Exp -- e1 += e2
  | StmtMinusEqWithLocation NodeLocation Exp Exp -- e1 -= e2
  | LetWithLocation NodeLocation Name (Maybe Ty) (Maybe Exp) -- local variable
  | BlockWithLocation NodeLocation Body -- lexical block
  | StmtExpWithLocation NodeLocation Exp -- expression level statements
  | ReturnWithLocation NodeLocation Exp -- return statements
  | MatchWithLocation NodeLocation [Exp] Equations -- pattern matching
  | AsmWithLocation NodeLocation YulBlock -- Yul block
  | IfWithLocation NodeLocation Exp Body Body -- If statement
  | ForWithLocation NodeLocation Stmt Exp Stmt Body -- for(init; cond; post) { body }
  deriving (Eq, Ord, Show, Data, Typeable)

pattern Assign :: Exp -> Exp -> Stmt
pattern Assign lhs rhs <- AssignWithLocation _ lhs rhs
  where
    Assign lhs rhs = AssignWithLocation unlocatedNode lhs rhs

pattern StmtPlusEq :: Exp -> Exp -> Stmt
pattern StmtPlusEq lhs rhs <- StmtPlusEqWithLocation _ lhs rhs
  where
    StmtPlusEq lhs rhs = StmtPlusEqWithLocation unlocatedNode lhs rhs

pattern StmtMinusEq :: Exp -> Exp -> Stmt
pattern StmtMinusEq lhs rhs <- StmtMinusEqWithLocation _ lhs rhs
  where
    StmtMinusEq lhs rhs = StmtMinusEqWithLocation unlocatedNode lhs rhs

pattern Let :: Name -> Maybe Ty -> Maybe Exp -> Stmt
pattern Let n ty value <- LetWithLocation _ n ty value
  where
    Let n ty value = LetWithLocation unlocatedNode n ty value

pattern Block :: Body -> Stmt
pattern Block body <- BlockWithLocation _ body
  where
    Block body = BlockWithLocation unlocatedNode body

pattern StmtExp :: Exp -> Stmt
pattern StmtExp exp <- StmtExpWithLocation _ exp
  where
    StmtExp exp = StmtExpWithLocation unlocatedNode exp

pattern Return :: Exp -> Stmt
pattern Return exp <- ReturnWithLocation _ exp
  where
    Return exp = ReturnWithLocation unlocatedNode exp

pattern Match :: [Exp] -> Equations -> Stmt
pattern Match exps equations <- MatchWithLocation _ exps equations
  where
    Match exps equations = MatchWithLocation unlocatedNode exps equations

pattern Asm :: YulBlock -> Stmt
pattern Asm block <- AsmWithLocation _ block
  where
    Asm block = AsmWithLocation unlocatedNode block

pattern If :: Exp -> Body -> Body -> Stmt
pattern If cond thenBody elseBody <- IfWithLocation _ cond thenBody elseBody
  where
    If cond thenBody elseBody = IfWithLocation unlocatedNode cond thenBody elseBody

pattern For :: Stmt -> Exp -> Stmt -> Body -> Stmt
pattern For initStmt cond postStmt body <- ForWithLocation _ initStmt cond postStmt body
  where
    For initStmt cond postStmt body = ForWithLocation unlocatedNode initStmt cond postStmt body

{-# COMPLETE Assign, StmtPlusEq, StmtMinusEq, Let, Block, StmtExp, Return, Match, Asm, If, For #-}

type Body = [Stmt]

locatedStmt :: SourceSpan -> Stmt -> Stmt
locatedStmt sourceSpan (Assign lhs rhs) = AssignWithLocation location lhs rhs
  where
    location = locatedNode sourceSpan
locatedStmt sourceSpan (StmtPlusEq lhs rhs) = StmtPlusEqWithLocation location lhs rhs
  where
    location = locatedNode sourceSpan
locatedStmt sourceSpan (StmtMinusEq lhs rhs) = StmtMinusEqWithLocation location lhs rhs
  where
    location = locatedNode sourceSpan
locatedStmt sourceSpan (Let n ty value) = LetWithLocation location n ty value
  where
    location = locatedNode sourceSpan
locatedStmt sourceSpan (Block body) = BlockWithLocation (locatedNode sourceSpan) body
locatedStmt sourceSpan (StmtExp exp) = StmtExpWithLocation (locatedNode sourceSpan) exp
locatedStmt sourceSpan (Return exp) = ReturnWithLocation (locatedNode sourceSpan) exp
locatedStmt sourceSpan (Match exps equations) = MatchWithLocation (locatedNode sourceSpan) exps equations
locatedStmt sourceSpan (Asm block) = AsmWithLocation (locatedNode sourceSpan) block
locatedStmt sourceSpan (If cond thenBody elseBody) = IfWithLocation (locatedNode sourceSpan) cond thenBody elseBody
locatedStmt sourceSpan (For initStmt cond postStmt body) = ForWithLocation (locatedNode sourceSpan) initStmt cond postStmt body

instance HasSourceSpan Stmt where
  sourceSpanOf (AssignWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (StmtPlusEqWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (StmtMinusEqWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (LetWithLocation location n ty value) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ty, sourceSpanOf value]
  sourceSpanOf (BlockWithLocation location body) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf body]
  sourceSpanOf (StmtExpWithLocation location exp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp]
  sourceSpanOf (ReturnWithLocation location exp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp]
  sourceSpanOf (MatchWithLocation location exps equations) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exps, sourceSpanOf equations]
  sourceSpanOf (AsmWithLocation location _) =
    sourceSpanOf location
  sourceSpanOf (IfWithLocation location cond thenBody elseBody) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf cond, sourceSpanOf thenBody, sourceSpanOf elseBody]
  sourceSpanOf (ForWithLocation location initStmt cond postStmt body) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf initStmt, sourceSpanOf cond, sourceSpanOf postStmt, sourceSpanOf body]

data Param
  = Typed Name Ty
  | Untyped Name
  deriving (Eq, Ord, Show, Data, Typeable)

instance HasSourceSpan Param where
  sourceSpanOf (Typed n ty) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf ty]
  sourceSpanOf (Untyped n) =
    sourceSpanOf n

-- expression syntax

data Exp
  = LitWithLocation NodeLocation Literal -- literal
  | ExpNameWithLocation NodeLocation (Maybe Exp) Name [Exp] -- function call or constructor
  | ExpVarWithLocation NodeLocation (Maybe Exp) Name -- variables or field access
  | ExpDotNameWithLocation NodeLocation Name [Exp] -- contextual constructor shorthand, e.g. .Some(1), .None
  | LamWithLocation NodeLocation [Param] Body (Maybe Ty) -- lambda-abstraction
  | TyExpWithLocation NodeLocation Exp Ty -- type annotation expression
  | ExpIndexedWithLocation NodeLocation Exp Exp -- e1[e2]
  | ExpPlusWithLocation NodeLocation Exp Exp -- e1 + e2
  | ExpMinusWithLocation NodeLocation Exp Exp -- e1 - e2
  | ExpTimesWithLocation NodeLocation Exp Exp -- e1 * e2
  | ExpDivideWithLocation NodeLocation Exp Exp -- e1 / e2
  | ExpModuloWithLocation NodeLocation Exp Exp -- e1 % e2
  | ExpLTWithLocation NodeLocation Exp Exp -- e1 < e2
  | ExpGTWithLocation NodeLocation Exp Exp -- e1 > e2
  | ExpLEWithLocation NodeLocation Exp Exp -- e1 <= e2
  | ExpGEWithLocation NodeLocation Exp Exp -- e1 >= e2
  | ExpEEWithLocation NodeLocation Exp Exp -- e1 == e2
  | ExpNEWithLocation NodeLocation Exp Exp -- e1 != e2
  | ExpLAndWithLocation NodeLocation Exp Exp -- e1 && e2
  | ExpLOrWithLocation NodeLocation Exp Exp -- e1 || e2
  | ExpLNotWithLocation NodeLocation Exp -- ! e
  | ExpCondWithLocation NodeLocation Exp Exp Exp -- if e1 then e2 else e3
  | ExpAtWithLocation NodeLocation Ty -- proxy sugar
  deriving (Eq, Ord, Show, Data, Typeable)

pattern Lit :: Literal -> Exp
pattern Lit lit <- LitWithLocation _ lit
  where
    Lit lit = LitWithLocation unlocatedNode lit

pattern ExpName :: Maybe Exp -> Name -> [Exp] -> Exp
pattern ExpName me n es <- ExpNameWithLocation _ me n es
  where
    ExpName me n es = ExpNameWithLocation unlocatedNode me n es

pattern ExpVar :: Maybe Exp -> Name -> Exp
pattern ExpVar me n <- ExpVarWithLocation _ me n
  where
    ExpVar me n = ExpVarWithLocation unlocatedNode me n

pattern ExpDotName :: Name -> [Exp] -> Exp
pattern ExpDotName n es <- ExpDotNameWithLocation _ n es
  where
    ExpDotName n es = ExpDotNameWithLocation unlocatedNode n es

pattern Lam :: [Param] -> Body -> Maybe Ty -> Exp
pattern Lam ps body ty <- LamWithLocation _ ps body ty
  where
    Lam ps body ty = LamWithLocation unlocatedNode ps body ty

pattern TyExp :: Exp -> Ty -> Exp
pattern TyExp exp ty <- TyExpWithLocation _ exp ty
  where
    TyExp exp ty = TyExpWithLocation unlocatedNode exp ty

pattern ExpIndexed :: Exp -> Exp -> Exp
pattern ExpIndexed lhs rhs <- ExpIndexedWithLocation _ lhs rhs
  where
    ExpIndexed lhs rhs = ExpIndexedWithLocation unlocatedNode lhs rhs

pattern ExpPlus :: Exp -> Exp -> Exp
pattern ExpPlus lhs rhs <- ExpPlusWithLocation _ lhs rhs
  where
    ExpPlus lhs rhs = ExpPlusWithLocation unlocatedNode lhs rhs

pattern ExpMinus :: Exp -> Exp -> Exp
pattern ExpMinus lhs rhs <- ExpMinusWithLocation _ lhs rhs
  where
    ExpMinus lhs rhs = ExpMinusWithLocation unlocatedNode lhs rhs

pattern ExpTimes :: Exp -> Exp -> Exp
pattern ExpTimes lhs rhs <- ExpTimesWithLocation _ lhs rhs
  where
    ExpTimes lhs rhs = ExpTimesWithLocation unlocatedNode lhs rhs

pattern ExpDivide :: Exp -> Exp -> Exp
pattern ExpDivide lhs rhs <- ExpDivideWithLocation _ lhs rhs
  where
    ExpDivide lhs rhs = ExpDivideWithLocation unlocatedNode lhs rhs

pattern ExpModulo :: Exp -> Exp -> Exp
pattern ExpModulo lhs rhs <- ExpModuloWithLocation _ lhs rhs
  where
    ExpModulo lhs rhs = ExpModuloWithLocation unlocatedNode lhs rhs

pattern ExpLT :: Exp -> Exp -> Exp
pattern ExpLT lhs rhs <- ExpLTWithLocation _ lhs rhs
  where
    ExpLT lhs rhs = ExpLTWithLocation unlocatedNode lhs rhs

pattern ExpGT :: Exp -> Exp -> Exp
pattern ExpGT lhs rhs <- ExpGTWithLocation _ lhs rhs
  where
    ExpGT lhs rhs = ExpGTWithLocation unlocatedNode lhs rhs

pattern ExpLE :: Exp -> Exp -> Exp
pattern ExpLE lhs rhs <- ExpLEWithLocation _ lhs rhs
  where
    ExpLE lhs rhs = ExpLEWithLocation unlocatedNode lhs rhs

pattern ExpGE :: Exp -> Exp -> Exp
pattern ExpGE lhs rhs <- ExpGEWithLocation _ lhs rhs
  where
    ExpGE lhs rhs = ExpGEWithLocation unlocatedNode lhs rhs

pattern ExpEE :: Exp -> Exp -> Exp
pattern ExpEE lhs rhs <- ExpEEWithLocation _ lhs rhs
  where
    ExpEE lhs rhs = ExpEEWithLocation unlocatedNode lhs rhs

pattern ExpNE :: Exp -> Exp -> Exp
pattern ExpNE lhs rhs <- ExpNEWithLocation _ lhs rhs
  where
    ExpNE lhs rhs = ExpNEWithLocation unlocatedNode lhs rhs

pattern ExpLAnd :: Exp -> Exp -> Exp
pattern ExpLAnd lhs rhs <- ExpLAndWithLocation _ lhs rhs
  where
    ExpLAnd lhs rhs = ExpLAndWithLocation unlocatedNode lhs rhs

pattern ExpLOr :: Exp -> Exp -> Exp
pattern ExpLOr lhs rhs <- ExpLOrWithLocation _ lhs rhs
  where
    ExpLOr lhs rhs = ExpLOrWithLocation unlocatedNode lhs rhs

pattern ExpLNot :: Exp -> Exp
pattern ExpLNot exp <- ExpLNotWithLocation _ exp
  where
    ExpLNot exp = ExpLNotWithLocation unlocatedNode exp

pattern ExpCond :: Exp -> Exp -> Exp -> Exp
pattern ExpCond cond thenExp elseExp <- ExpCondWithLocation _ cond thenExp elseExp
  where
    ExpCond cond thenExp elseExp = ExpCondWithLocation unlocatedNode cond thenExp elseExp

pattern ExpAt :: Ty -> Exp
pattern ExpAt ty <- ExpAtWithLocation _ ty
  where
    ExpAt ty = ExpAtWithLocation unlocatedNode ty

{-# COMPLETE Lit, ExpName, ExpVar, ExpDotName, Lam, TyExp, ExpIndexed, ExpPlus, ExpMinus, ExpTimes, ExpDivide, ExpModulo, ExpLT, ExpGT, ExpLE, ExpGE, ExpEE, ExpNE, ExpLAnd, ExpLOr, ExpLNot, ExpCond, ExpAt #-}

locatedExp :: SourceSpan -> Exp -> Exp
locatedExp sourceSpan (Lit lit) = LitWithLocation location lit
  where
    location = locatedNode sourceSpan
locatedExp sourceSpan (ExpName me n es) = ExpNameWithLocation (locatedNode sourceSpan) me n es
locatedExp sourceSpan (ExpVar me n) = ExpVarWithLocation (locatedNode sourceSpan) me n
locatedExp sourceSpan (ExpDotName n es) = ExpDotNameWithLocation (locatedNode sourceSpan) n es
locatedExp sourceSpan (Lam ps body ty) = LamWithLocation (locatedNode sourceSpan) ps body ty
locatedExp sourceSpan (TyExp exp ty) = TyExpWithLocation (locatedNode sourceSpan) exp ty
locatedExp sourceSpan (ExpIndexed lhs rhs) = ExpIndexedWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpPlus lhs rhs) = ExpPlusWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpMinus lhs rhs) = ExpMinusWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpTimes lhs rhs) = ExpTimesWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpDivide lhs rhs) = ExpDivideWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpModulo lhs rhs) = ExpModuloWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpLT lhs rhs) = ExpLTWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpGT lhs rhs) = ExpGTWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpLE lhs rhs) = ExpLEWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpGE lhs rhs) = ExpGEWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpEE lhs rhs) = ExpEEWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpNE lhs rhs) = ExpNEWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpLAnd lhs rhs) = ExpLAndWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpLOr lhs rhs) = ExpLOrWithLocation (locatedNode sourceSpan) lhs rhs
locatedExp sourceSpan (ExpLNot exp) = ExpLNotWithLocation (locatedNode sourceSpan) exp
locatedExp sourceSpan (ExpCond cond thenExp elseExp) = ExpCondWithLocation (locatedNode sourceSpan) cond thenExp elseExp
locatedExp sourceSpan (ExpAt ty) = ExpAtWithLocation (locatedNode sourceSpan) ty

instance HasSourceSpan Exp where
  sourceSpanOf (LitWithLocation location _) = sourceSpanOf location
  sourceSpanOf (ExpNameWithLocation location me n es) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf me, sourceSpanOf n, sourceSpanOf es]
  sourceSpanOf (ExpVarWithLocation location me n) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf me, sourceSpanOf n]
  sourceSpanOf (ExpDotNameWithLocation location n es) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf es]
  sourceSpanOf (LamWithLocation location ps body ty) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf ps, sourceSpanOf body, sourceSpanOf ty]
  sourceSpanOf (TyExpWithLocation location exp ty) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp, sourceSpanOf ty]
  sourceSpanOf (ExpIndexedWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpPlusWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpMinusWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpTimesWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpDivideWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpModuloWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpLTWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpGTWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpLEWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpGEWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpEEWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpNEWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpLAndWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpLOrWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (ExpLNotWithLocation location exp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp]
  sourceSpanOf (ExpCondWithLocation location cond thenExp elseExp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf cond, sourceSpanOf thenExp, sourceSpanOf elseExp]
  sourceSpanOf (ExpAtWithLocation location ty) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf ty]

-- pattern matching equations

data Pat
  = PatWithLocation NodeLocation Name [Pat]
  | PatDotWithLocation NodeLocation Name [Pat]
  | PWildcardWithLocation NodeLocation
  | PLitWithLocation NodeLocation Literal
  deriving (Eq, Ord, Show, Data, Typeable)

pattern Pat :: Name -> [Pat] -> Pat
pattern Pat n ps <- PatWithLocation _ n ps
  where
    Pat n ps = PatWithLocation unlocatedNode n ps

pattern PatDot :: Name -> [Pat] -> Pat
pattern PatDot n ps <- PatDotWithLocation _ n ps
  where
    PatDot n ps = PatDotWithLocation unlocatedNode n ps

pattern PWildcard :: Pat
pattern PWildcard <- PWildcardWithLocation _
  where
    PWildcard = PWildcardWithLocation unlocatedNode

pattern PLit :: Literal -> Pat
pattern PLit lit <- PLitWithLocation _ lit
  where
    PLit lit = PLitWithLocation unlocatedNode lit

{-# COMPLETE Pat, PatDot, PWildcard, PLit #-}

locatedPat :: SourceSpan -> Pat -> Pat
locatedPat sourceSpan (Pat n ps) = PatWithLocation (locatedNode sourceSpan) n ps
locatedPat sourceSpan (PatDot n ps) = PatDotWithLocation (locatedNode sourceSpan) n ps
locatedPat sourceSpan PWildcard = PWildcardWithLocation (locatedNode sourceSpan)
locatedPat sourceSpan (PLit lit) = PLitWithLocation (locatedNode sourceSpan) lit

instance HasSourceSpan Pat where
  sourceSpanOf (PatWithLocation location n ps) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ps]
  sourceSpanOf (PatDotWithLocation location n ps) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ps]
  sourceSpanOf (PWildcardWithLocation location) = sourceSpanOf location
  sourceSpanOf (PLitWithLocation location _) = sourceSpanOf location

-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)

pairTy :: Ty -> Ty -> Ty
pairTy t1 t2 = TyCon "pair" [t1, t2]

funtype :: [Ty] -> Ty -> Ty
funtype ts t = foldr (:->) t ts
