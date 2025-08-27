{
module Solcore.Frontend.Parser.SolcoreParser where

import Data.Either
import Data.List.NonEmpty (NonEmpty, cons, singleton)

import Solcore.Frontend.Lexer.SolcoreLexer hiding (lexer)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Primitives.Primitives hiding (pairTy)
import Language.Yul
import System.FilePath
}


%name parser CompilationUnit
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}

%token
      identifier {Token _ (TIdent $$)}
      number     {Token _ (TNumber $$)}
      stringlit  {Token _ (TString $$)}
      'contract' {Token _ TContract}
      'import'   {Token _ TImport}
      'let'      {Token _ TLet}
      '='        {Token _ TEq}
      '.'        {Token _ TDot}
      'forall'   {Token _ TForall}
      'class'    {Token _ TClass}
      'instance' {Token _ TInstance}
      'if'       {Token _ TIf}
      'else'     {Token _ TElse}
      'for'      {Token _ TFor}
      'switch'   {Token _ TSwitch}
      'case'     {Token _ TCase}
      'default'  {Token _ TDefault}
      'leave'    {Token _ TLeave}
      'continue' {Token _ TContinue}
      'break'    {Token _ TBreak}
      'assembly' {Token _ TAssembly}
      'data'     {Token _ TData}
      'match'    {Token _ TMatch}
      'function' {Token _ TFunction}
      'constructor' {Token _ TConstructor}
      'return'   {Token _ TReturn}
      'lam'      {Token _ TLam}
      'type'     {Token _ TType}
      'no-patterson-condition' {Token _ TNoPattersonCondition}
      'no-coverage-condition'  {Token _ TNoCoverageCondition}
      'no-bounded-variable-condition' {Token _ TNoBoundVariableCondition}
      'pragma'      {Token _ TPragma}
      ';'        {Token _ TSemi}
      ':='       {Token _ TYAssign}
      ':'        {Token _ TColon}
      ','        {Token _ TComma}
      '->'       {Token _ TArrow}
      '_'        {Token _ TWildCard}
      '=>'       {Token _ TDArrow}
      '('        {Token _ TLParen}
      ')'        {Token _ TRParen}
      '{'        {Token _ TLBrace}
      '}'        {Token _ TRBrace}
      '|'        {Token _ TBar}
      '['        {Token _ TLBrack}
      ']'        {Token _ TRBrack}

%expect 0

%%
-- compilation unit definition

CompilationUnit :: { CompUnit }
CompilationUnit : ImportList TopDeclList          { CompUnit $1 $2 }

ImportList :: { [Import] }
ImportList : ImportList Import                     { $2 : $1 }
           | {- empty -}                           { [] }

Import :: { Import }
Import : 'import' Name ';'                         { Import $2 }

TopDeclList :: { [TopDecl] }
TopDeclList : TopDecl TopDeclList                  { $1 : $2 }
             | {- empty -}                         { [] }


-- top level declarations

TopDecl :: { TopDecl }
TopDecl : Contract                                 {TContr $1}
        | Function                                 {TFunDef $1}
        | ClassDef                                 {TClassDef $1}
        | InstDef                                  {TInstDef $1}
        | DataDef                                  {TDataDef $1}
        | TypeSynonym                              {TSym $1}
        | Pragma                                   {TPragmaDecl $1}

-- pragmas

Pragma :: {Pragma}
Pragma : 'pragma' 'no-coverage-condition' Status ';'
            {Pragma NoCoverageCondition $3 }
       | 'pragma' 'no-patterson-condition' Status ';'
           {Pragma NoPattersonCondition $3}
       | 'pragma' 'no-bounded-variable-condition' Status ';'
          { Pragma NoBoundVariableCondition $3}

Status :: {PragmaStatus}
Status : NameList       {DisableFor $1}
       | {- empty -}    {DisableAll}

NameList :: {NonEmpty Name}
NameList : Name ',' NameList { cons $1 $3 }
         | Name              { singleton $1 }

-- contracts

Contract :: { Contract }
Contract : 'contract' Name OptParam '{' DeclList '}' { Contract $2 $3 $5 }

DeclList :: { [ContractDecl] }
DeclList : Decl DeclList                           { $1 : $2 }
         | {- empty -}                             { [] }

-- declarations

Decl :: { ContractDecl }
Decl : FieldDef                                    {CFieldDecl $1}
     | DataDef                                     {CDataDecl $1}
     | Function                                    {CFunDecl $1}
     | Constructor                                 {CConstrDecl $1}

-- type synonym

TypeSynonym :: {TySym}
TypeSynonym : 'type' Name OptParam '=' Type ';'    {TySym $2 $3 $5}

-- fields

FieldDef :: { Field }
FieldDef : Name ':' Type InitOpt ';'               {Field $1 $3 $4}

-- algebraic data types

DataDef :: { DataTy }
DataDef : 'data' Name OptParam DataCons ';'        {DataTy $2 $3 $4}

DataCons :: {[Constr]}
DataCons : '=' Constrs                             {$2}
         | {- empty -}                             {[]}

Constrs :: {[Constr]}
Constrs : Constr '|' Constrs                       {$1 : $3}
        | Constr                                   {[$1]}

Constr :: { Constr }
Constr : Name OptTypeParam                          { Constr $1 $2 }

-- class definitions

ClassDef :: { Class }
ClassDef
 : SigPrefix 'class' Var ':' Name OptParam ClassBody {Class (fst $1) (snd $1) $5 $6 $3 $7}

ClassBody :: {[Signature]}
ClassBody : '{' Signatures '}'                     {$2}

OptParam :: { [Ty] }
OptParam :  '(' VarCommaList ')'                   {$2}
         | {- empty -}                             {[]}

VarCommaList :: { [Ty] }
VarCommaList : Var ',' VarCommaList                {$1 : $3}
             | Var                                 {[$1]}

ConstraintList :: { [Pred] }
ConstraintList : Constraint ',' ConstraintList     {$1 : $3}
               | Constraint                        {[$1]}

Constraint :: { Pred }
Constraint : Type ':' Name OptTypeParam             {InCls $3 $1 $4}

Signatures :: { [Signature ] }
Signatures : Signature ';' Signatures              {$1 : $3}
           | {- empty -}                           {[]}

Signature :: { Signature }
Signature : SigPrefix 'function' Name '(' ParamList ')' OptRetTy {Signature (fst $1) (snd $1) $3 $5 $7}

SigPrefix :: {([Ty], [Pred])}
SigPrefix : 'forall' Tyvars '.' ConstraintList '=>' {($2, $4)}
          | 'forall' Tyvars '.'                     {($2, [])}
          | {- empty -}                             {([], [])}

ParamList :: { [Param] }
ParamList : Param                                  {[$1]}
          | Param  ',' ParamList                   {$1 : $3}
          | {- empty -}                            {[]}

Param :: { Param }
Param : Name ':' Type                              {Typed $1 $3}
      | Name                                       {Untyped $1}

-- instance declarations

InstDef :: { Instance }
InstDef : SigPrefix DefaultOpt 'instance' Type ':' Name OptTypeParam InstBody { Instance $2 (fst $1) (snd $1) $6 $7 $4 $8 }

DefaultOpt :: { Bool }
DefaultOpt : 'default'                        {True}
           | {- empty -}                      {False}

OptTypeParam :: { [Ty] }
OptTypeParam : '(' TypeCommaList ')'          {$2}
             | {- empty -}                    {[]}

TypeCommaList :: { [Ty] }
TypeCommaList : Type ',' TypeCommaList             {$1 : $3}
              | Type                               {[$1]}
              | {- empty -}                        { [] }

Tyvars :: {[Ty]}
Tyvars : Name Tyvars { (TyCon $1 []) : $2}
       | {-empty-}     {[]}

Functions :: { [FunDef] }
Functions : Function Functions                     {$1 : $2}
          | {- empty -}                            {[]}

InstBody :: {[FunDef]}
InstBody : '{' Functions '}'                       {$2}

-- Function declaration

Function :: { FunDef }
Function : Signature Body {FunDef $1 $2}

OptRetTy :: { Maybe Ty }
OptRetTy : '->' Type                               {Just $2}
         | {- empty -}                             {Nothing}

-- Contract constructor

Constructor :: { Constructor }
Constructor : 'constructor' '(' ParamList ')' Body {Constructor $3 $5}

-- Function body

Body :: { [Stmt] }
Body : '{' StmtList '}'                            {$2}

StmtList :: { [Stmt] }
StmtList : Stmt StmtList                       {$1 : $2}
         | {- empty -}                             {[]}

-- Statements

Stmt :: { Stmt }
Stmt : Expr '=' Expr ';'                              {Assign $1 $3}
     | 'let' Name ':' Type InitOpt ';'                {Let $2 (Just $4) $5}
     | 'let' Name InitOpt ';'                         {Let $2 Nothing $3}
     | Expr ';'                                       {StmtExp $1}
     | 'return' Expr ';'                              {Return $2}
     | 'match' MatchArgList '{' Equations  '}'        {Match $2 $4}
     | AsmBlock                                       {Asm $1}
     | 'if' '(' Expr ')' Body %shift                  {If $3 $5 []}
     | 'if' '(' Expr ')' Body 'else' Body             {If $3 $5 $7}


MatchArgList :: {[Exp]}
MatchArgList : Expr                                {[$1]}
             | Expr ',' MatchArgList               {$1 : $3}

InitOpt :: {Maybe Exp}
InitOpt : {- empty -}                              {Nothing}
        | '=' Expr                                 {Just $2}

-- Expressions

Expr :: { Exp }
Expr : Name FunArgs                                {ExpName Nothing $1 $2}
     | Literal                                     {Lit $1}
     | '(' Expr ')'                                {$2}
     | Expr '.' Name FunArgs                       {ExpName (Just $1) $3 $4}
     | Name                                        {ExpVar Nothing $1}
     | Expr '.' Name                               {ExpVar (Just $1) $3}
     | 'lam' '(' ParamList ')' OptRetTy Body       {Lam $3 $6 $5}
     | Expr ':' Type                               {TyExp $1 $3}
     | '(' TupleArgs ')'                           {tupleExp $2}
     | Expr '[' Expr ']'                           {ExpIndexed $1 $3 }

TupleArgs :: { [Exp] }
TupleArgs : Expr ',' Expr                          {[$1, $3]}
          | Expr ',' TupleArgs                     {$1 : $3}
          | {- empty -}                            {[]}

FunArgs :: {[Exp]}
FunArgs : '(' ExprCommaList ')'                    {$2}

ExprCommaList :: { [Exp] }
ExprCommaList : Expr                               {[$1]}
              | {- empty -}                        {[]}
              | Expr ',' ExprCommaList             {$1 : $3}

-- Pattern matching equations

Equations :: { [([Pat], [Stmt])]}
Equations : Equation Equations                     {$1 : $2}
          | {- empty -}                            {[]}

Equation :: { ([Pat], [Stmt]) }
Equation : '|' PatCommaList '=>' StmtList          {($2, $4)}

PatCommaList :: { [Pat] }
PatCommaList : Pattern                             {[$1]}
             | Pattern ',' PatCommaList            {$1 : $3}

Pattern :: { Pat }
Pattern : Name PatternList                         {Pat $1 $2}
        | '_'                                      {PWildcard}
        | Literal                                  {PLit $1}
        | '(' Pattern ')'                          {$2}
        | PatternList                              {Pat (Name "pair") $1}

PatternList :: {[Pat]}
PatternList : '(' PatList ')'                      {$2}
            | {- empty -}                          {[]}

PatList :: { [Pat] }
PatList : Pattern %shift                           {[$1]}
        | Pattern ',' PatList                      {$1 : $3}

-- literals

Literal :: { Literal }
Literal : number                                   {IntLit $ toInteger $1}
        | stringlit                                {StrLit $1}

-- basic type definitions

Type :: { Ty }
Type : Name OptTypeParam                            {TyCon $1 $2}
     | LamType                                      {uncurry funtype $1}
     | TupleTy                                      {$1}

TupleTy :: { Ty }
TupleTy : '(' TypeCommaList ')'                     {mkTupleTy $2}

LamType :: {([Ty], Ty)}
LamType : '(' TypeCommaList ')' '->' Type          {($2, $5)}

Var :: { Ty }
Var : Name                                         {TyCon $1 []}

Name :: { Name }
Name : identifier                               { Name $1 }
     | QualName %shift                          { QualName (fst $1) (snd $1) }

QualName :: { (Name, String) }
QualName : QualName '.' identifier              { (QualName (fst $1) (snd $1), $3)}

-- Yul statments and blocks

AsmBlock :: {YulBlock}
AsmBlock : 'assembly' YulBlock                     {$2}

YulBlock :: {YulBlock}
YulBlock : '{' YulStmts '}'                        {$2}

YulStmts :: {[YulStmt]}
YulStmts : YulStmt OptSemi YulStmts                {$1 : $3}
         | {- empty -}                             {[]}

YulStmt :: {YulStmt}
YulStmt : YulAssignment                            {$1}
        | YulBlock                                 {YBlock $1}
        | YulVarDecl                               {$1}
        | YulExp                                   {YExp $1}
        | YulIf                                    {$1}
        | YulSwitch                                {$1}
        | YulFor                                   {$1}
        | 'continue'                               {YContinue}
        | 'break'                                  {YBreak}
        | 'leave'                                  {YLeave}

YulFor :: {YulStmt}
YulFor : 'for' YulBlock YulExp YulBlock YulBlock   {YFor $2 $3 $4 $5}

YulSwitch :: {YulStmt}
YulSwitch : 'switch' YulExp YulCases YulDefault    {YSwitch $2 $3 $4}

YulCases :: {YulCases}
YulCases : YulCase YulCases                        {$1 : $2}
         | {- empty -}                             {[]}

YulCase :: {(YLiteral, YulBlock)}
YulCase : 'case' YulLiteral YulBlock                  {($2, $3)}

YulDefault :: {Maybe YulBlock}
YulDefault : 'default' YulBlock                    {Just $2}
           | {- empty -}                           {Nothing}

YulIf :: {YulStmt}
YulIf : 'if' YulExp YulBlock                       {YIf $2 $3}

YulVarDecl :: {YulStmt}
YulVarDecl : 'let' IdentifierList YulOptAss     {YLet $2 $3}

YulOptAss :: {Maybe YulExp}
YulOptAss : ':=' YulExp                            {Just $2}
          | {- empty -}                            {Nothing}

YulAssignment :: {YulStmt}
YulAssignment : IdentifierList ':=' YulExp         {YAssign $1 $3}

IdentifierList :: {[Name]}
IdentifierList : Name                              {[$1]}
               | Name ',' IdentifierList           {$1 : $3}



YulExp :: {YulExp}
YulExp : YulLiteral                                   {YLit $1}
       | Name                                      {YIdent $1}
       | Name YulFunArgs                           {YCall $1 $2}

YulFunArgs :: {[YulExp]}
YulFunArgs : '(' YulExpCommaList ')'               {$2}

YulExpCommaList :: { [YulExp] }
YulExpCommaList : YulExp                           {[$1]}
              | {- empty -}                        {[]}
              | YulExp ',' YulExpCommaList         {$1 : $3}

YulLiteral :: { YLiteral }
YulLiteral : number                                {YulNumber $ toInteger $1}
        | stringlit                                {YulString $1}

OptSemi :: { () }
OptSemi : ';'                                      { () }
        | {- empty -}                              { () }

{

moduleParser :: String -> String -> IO (Either String CompUnit)
moduleParser dir content
  = do
      let r = runAlex content parser
      case r of
        Left err -> pure $ Left err
        Right (CompUnit imps ds) -> do
           ds' <- loadImports dir imps
           pure $ either Left (\ ds1 -> Right $ CompUnit imps (ds1 ++ ds)) ds'

loadImports :: String -> [Import] -> IO (Either String [TopDecl])
loadImports dir imps =
  do
    let paths = map (toFilePath dir . unImport) imps
    contents <- mapM readFile paths
    rs <- mapM (moduleParser dir) contents
    let (errs, asts) = partitionEithers rs
    case errs of
      [] -> do
        let ds' = concatMap topDeclsFrom asts
        pure (Right ds')
      (err : _) -> pure (Left err)

toFilePath :: FilePath -> Name -> FilePath
toFilePath base =
  (base </>) . (<.> "solc") . foldr step "" . show
 where
  step c ac
    | c == '.' = pathSeparator : ac
    | otherwise = c : ac

topDeclsFrom :: CompUnit -> [TopDecl]
topDeclsFrom (CompUnit _ ds) = ds

unitPCon :: Pat
unitPCon = Pat (Name "()") []

mkTupleTy :: [Ty] -> Ty
mkTupleTy [] = TyCon (Name "()") []
mkTupleTy ts = foldr1 pairTy ts

pairExp :: Exp -> Exp -> Exp
pairExp e1 e2 = ExpName Nothing (Name "pair") [e1, e2]

tupleExp :: [Exp] -> Exp
tupleExp [] = ExpName Nothing (Name "()") []
tupleExp [t1] = t1
tupleExp [t1, t2] = pairExp t1 t2
tupleExp (t1 : ts) = pairExp t1 (tupleExp ts)

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
