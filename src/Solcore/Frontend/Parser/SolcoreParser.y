{
module Solcore.Frontend.Parser.SolcoreParser where

import Data.List.NonEmpty (NonEmpty, cons, singleton)

import Solcore.Frontend.Lexer.SolcoreLexer hiding (lexer)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Primitives.Primitives hiding (pairTy)
import Language.Yul
import Solcore.Diagnostics

}


%name parser CompilationUnit
%monad {Alex}{(>>=)}{pure}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}

%token
      identifier {Token _ (TIdent $$)}
      number     {Token _ (TNumber $$)}
      stringlit  {Token _ (TString $$)}
      'contract' {Token _ TContract}
      'import'   {Token _ TImport}
      'export'   {Token _ TExport}
      'hiding'   {Token _ THiding}
      'as'       {Token _ TAs}
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
      'assembly' {TokenWithSpan $$ _ TAssembly}
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
      '_'        {TokenWithSpan $$ _ TWildCard}
      '=>'       {Token _ TDArrow}
      '('        {Token _ TLParen}
      ')'        {Token _ TRParen}
      '{'        {Token _ TLBrace}
      '}'        {Token _ TRBrace}
      '|'        {Token _ TBar}
      '['        {Token _ TLBrack}
      ']'        {Token _ TRBrack}
      '<'        {Token _ TLT}
      '>'        {Token _ TGT}
      '>='       {Token _ TGE}
      '<='       {Token _ TLE}
      '!='       {Token _ TNE}
      '=='       {Token _ TEE}
      '&&'       {Token _ TLAnd}
      '||'       {Token _ TLOr}
      '!'        {Token _ TLNot}
      '+'        {Token _ TPlus}
      '-'        {Token _ TMinus}
      '*'        {Token _ TTimes}
      '/'        {Token _ TDivide}
      '%'        {Token _ TModulo}
      '+='       {Token _ TPlusEq}
      '-='       {Token _ TMinusEq}
      'then'     {Token _ TThen}
      '@'        {Token _ TAt}

%nonassoc '+=' '-='
%left     ':'
%left     '||'
%left     '&&'
%nonassoc '!'
%nonassoc '==' '!='
%nonassoc '<' '>' '<=' '>='
%left     '+' '-'
%left     '*' '/' '%'
%left     '['
%left     '.'
%right    'if'
%right    'else'

-- One intentional shift/reduce conflict: shift `.` so `Foo.Bar` stays a
-- qualified type name instead of ending the type before the dot.
%expect 1

%%
-- compilation unit definition

CompilationUnit :: { CompUnit }
CompilationUnit : ImportList TopDeclList          { CompUnit $1 $2 }

ImportList :: { [Import] }
ImportList : ImportList Import                     { $1 ++ [$2] }
           | {- empty -}                           { [] }

Import :: { Import }
Import : 'import' ModName ';'                      { ImportModule (classifyImportPath $2) }
       | 'import' ModName 'as' Name ';'            { ImportAlias (classifyImportPath $2) $4 }
       | 'import' ModName '.' '{' ImportItems '}' ImportHiding ';'
           { ImportOnly (classifyImportPath $2) (SelectItems $5 $7) }
       | 'import' '@' identifier '.' ModName ';'   { ImportModule (ExternalPath (nameFromIdent $3) $5) }
       | 'import' '@' identifier '.' ModName 'as' Name ';' { ImportAlias (ExternalPath (nameFromIdent $3) $5) $7 }
       | 'import' '@' identifier '.' ModName '.' '{' ImportItems '}' ImportHiding ';'
           { ImportOnly (ExternalPath (nameFromIdent $3) $5) (SelectItems $8 $10) }

ImportItems :: { [ItemSelectorEntry] }
ImportItems : ImportItemList                       { $1 }

ImportItemList :: { [ItemSelectorEntry] }
ImportItemList : ImportItem ',' ImportItemList     { $1 : $3 }
               | ImportItem                        { [$1] }

ImportItem :: { ItemSelectorEntry }
ImportItem : '*'                                   { SelectAllItems }
           | ItemName                              { SelectItem $1 }

ImportHiding :: { [Name] }
ImportHiding : 'hiding' '{' HidingItemList '}'     { $3 }
             | {- empty -}                         { [] }

HidingItemList :: { [Name] }
HidingItemList : ItemName ',' HidingItemList       { $1 : $3 }
               | ItemName                          { [$1] }

ModName :: { Name }
ModName : identifier                               { nameFromIdent $1 }
        | ModName '.' identifier                   { qualNameFromIdent $1 $3 }

ItemName :: { Name }
ItemName : identifier                              { nameFromIdent $1 }

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
        | ExportDecl                               {TExportDecl $1}
        | Pragma                                   {TPragmaDecl $1}

ExportDecl :: { Export }
ExportDecl : 'export' '{' ExportItems '}' ';'      { ExportList $3 }
           | 'export' ModName ExportTail           { $3 (classifyImportPath $2) }
           | 'export' '@' identifier '.' ModName ExportTail
               { $6 (ExternalPath (nameFromIdent $3) $5) }

ExportTail :: { ModulePath -> Export }
ExportTail : ';'                                   { ExportModule }
           | 'as' Name ';'                         { \path -> ExportModuleAs path $2 }
           | '.' '{' ExportFromItems '}' ';'       { \path -> ExportItemsFrom path (SelectExportItems $3) }
           | '.' '*' ';'                           { \path -> ExportItemsFrom path (SelectExportItems [SelectExportAllItems]) }

ExportItems :: { [ExportSpec] }
ExportItems : ExportListItems                      { $1 }
            | {- empty -}                          { [] }

ExportListItems :: { [ExportSpec] }
ExportListItems : ExportListItem ',' ExportListItems { $1 : $3 }
                | ExportListItem                   { [$1] }

ExportListItem :: { ExportSpec }
ExportListItem : '*'                               { ExportAll }
               | ItemName                          { ExportName $1 }
               | ItemName '(' ExportConstructorItems ')' { ExportNameWithConstructors $1 $3 }
               | ModName '.' '*'                   { ExportModuleAll (classifyImportPath $1) }
               | '@' identifier '.' ModName '.' '*' { ExportModuleAll (ExternalPath (nameFromIdent $2) $4) }

ExportFromItems :: { [ExportSelectorEntry] }
ExportFromItems : ExportFromItemList               { $1 }
                | {- empty -}                      { [] }

ExportFromItemList :: { [ExportSelectorEntry] }
ExportFromItemList : ExportFromItem ',' ExportFromItemList { $1 : $3 }
                   | ExportFromItem                { [$1] }

ExportFromItem :: { ExportSelectorEntry }
ExportFromItem : '*'                               { SelectExportAllItems }
               | ItemName                          { SelectExportItem $1 }
               | ItemName '(' ExportConstructorItems ')' { SelectExportConstructors $1 $3 }

ExportConstructorItems :: { ConstructorSelector }
ExportConstructorItems : '*'                       { SelectAllConstructors }
                       | ConstructorNameList       { SelectConstructors $1 }

ConstructorNameList :: { [Name] }
ConstructorNameList : ItemName ',' ConstructorNameList { $1 : $3 }
                    | ItemName                     { [$1] }

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
-- Proposed Rust-style short return, e.g `function d(x) { 2*x }`
         | Signature '{' Expr '}' {FunDef $1 [locatedFrom $3 locatedStmt (Return $3)]}

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
Stmt : Expr '=' Expr ';'                              {locatedBetween $1 $3 locatedStmt (Assign $1 $3)}
     | Expr '+=' Expr ';'                             {locatedBetween $1 $3 locatedStmt (StmtPlusEq $1 $3)}
     | Expr '-=' Expr ';'                             {locatedBetween $1 $3 locatedStmt (StmtMinusEq $1 $3)}
     | 'let' Name ':' Type InitOpt ';'                {locatedBetween $2 (Just $4, $5) locatedStmt (Let $2 (Just $4) $5)}
     | 'let' Name InitOpt ';'                         {locatedBetween $2 $3 locatedStmt (Let $2 Nothing $3)}
     | Expr ';'                                       {locatedFrom $1 locatedStmt (StmtExp $1)}
     | 'return' Expr ';'                              {locatedFrom $2 locatedStmt (Return $2)}
     | 'match' MatchArgList '{' Equations  '}'        {locatedBetween $2 $4 locatedStmt (Match $2 $4)}
     | AsmBlock                                       {locatedValueWith locatedStmt $1 Asm}
     | 'if' '(' Expr ')' Body %shift                  {locatedBetween $3 $5 locatedStmt (If $3 $5 [])}
     | 'if' '(' Expr ')' Body 'else' Body             {locatedBetween $3 ($5, $7) locatedStmt (If $3 $5 $7)}
        | 'for' '(' ForInitStmt ';' Expr ';' ForPostStmt ')' Body {locatedBetween $3 ($5, $7, $9) locatedStmt (For $3 $5 $7 $9)}

ForInitStmt :: { Stmt }
ForInitStmt : Expr '=' Expr                               {locatedBetween $1 $3 locatedStmt (Assign $1 $3)}
            | Expr '+=' Expr                              {locatedBetween $1 $3 locatedStmt (StmtPlusEq $1 $3)}
            | Expr '-=' Expr                              {locatedBetween $1 $3 locatedStmt (StmtMinusEq $1 $3)}
            | 'let' Name ':' Type InitOpt                 {locatedBetween $2 (Just $4, $5) locatedStmt (Let $2 (Just $4) $5)}
            | 'let' Name InitOpt                          {locatedBetween $2 $3 locatedStmt (Let $2 Nothing $3)}
            | Expr                                        {locatedFrom $1 locatedStmt (StmtExp $1)}

ForPostStmt :: { Stmt }
ForPostStmt : Expr '=' Expr                               {locatedBetween $1 $3 locatedStmt (Assign $1 $3)}
            | Expr '+=' Expr                              {locatedBetween $1 $3 locatedStmt (StmtPlusEq $1 $3)}
            | Expr '-=' Expr                              {locatedBetween $1 $3 locatedStmt (StmtMinusEq $1 $3)}
            | Expr                                        {locatedFrom $1 locatedStmt (StmtExp $1)}


MatchArgList :: {[Exp]}
MatchArgList : Expr                                {[$1]}
             | Expr ',' MatchArgList               {$1 : $3}

InitOpt :: {Maybe Exp}
InitOpt : {- empty -}                              {Nothing}
        | '=' Expr                                 {Just $2}

-- Expressions

Expr :: { Exp }
Expr : Name FunArgs                                {locatedBetween $1 $2 locatedExp (ExpName Nothing $1 $2)}
     | Literal                                     {locatedValueWith locatedExp $1 Lit}
     | '(' Expr ')'                                {$2}
     | '.' Name FunArgs                            {locatedBetween $2 $3 locatedExp (ExpDotName $2 $3)}
     | Expr '.' Name FunArgs                       {locatedBetween $1 ($3, $4) locatedExp (ExpName (Just $1) $3 $4)}
     | Name                                        {locatedFrom $1 locatedExp (ExpVar Nothing $1)}
     | '.' Name                                    {locatedFrom $2 locatedExp (ExpDotName $2 [])}
     | Expr '.' Name                               {locatedBetween $1 $3 locatedExp (ExpVar (Just $1) $3)}
     | 'lam' '(' ParamList ')' OptRetTy Body       {locatedBetween $3 ($5, $6) locatedExp (Lam $3 $6 $5)}
     | Expr ':' Type                               {locatedBetween $1 $3 locatedExp (TyExp $1 $3)}
     | '(' TupleArgs ')'                           {locatedFrom $2 locatedExp (tupleExp $2)}
     | Expr '[' Expr ']'                           {locatedBetween $1 $3 locatedExp (ExpIndexed $1 $3)}
     | Expr '+' Expr                               {locatedBetween $1 $3 locatedExp (ExpPlus $1 $3)}
     | Expr '-' Expr                               {locatedBetween $1 $3 locatedExp (ExpMinus $1 $3)}
     | Expr '*' Expr                               {locatedBetween $1 $3 locatedExp (ExpTimes $1 $3)}
     | Expr '/' Expr                               {locatedBetween $1 $3 locatedExp (ExpDivide $1 $3)}
     | Expr '%' Expr                               {locatedBetween $1 $3 locatedExp (ExpModulo $1 $3)}
     | Expr '<' Expr                               {locatedBetween $1 $3 locatedExp (ExpLT $1 $3)}
     | Expr '>' Expr                               {locatedBetween $1 $3 locatedExp (ExpGT $1 $3)}
     | Expr '<=' Expr                              {locatedBetween $1 $3 locatedExp (ExpLE $1 $3)}
     | Expr '>=' Expr                              {locatedBetween $1 $3 locatedExp (ExpGE $1 $3)}
     | Expr '==' Expr                              {locatedBetween $1 $3 locatedExp (ExpEE $1 $3)}
     | Expr '!=' Expr                              {locatedBetween $1 $3 locatedExp (ExpNE $1 $3)}
     | Expr '&&' Expr                              {locatedBetween $1 $3 locatedExp (ExpLAnd $1 $3)}
     | Expr '||' Expr                              {locatedBetween $1 $3 locatedExp (ExpLOr $1 $3)}
     | '!' Expr                                    {locatedFrom $2 locatedExp (ExpLNot $2)}
     | Conditional                                 {$1}
     | '@' Type                                    {locatedFrom $2 locatedExp (ExpAt $2)}

Conditional :: { Exp }
Conditional : 'if' Expr 'then' Expr 'else' Expr    {locatedBetween $2 $6 locatedExp (ExpCond $2 $4 $6)}

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
Pattern : TypeName PatternList                     {locatedBetween $1 $2 locatedPat (Pat $1 $2)}
        | '.' Name PatternList                     {locatedBetween $2 $3 locatedPat (PatDot $2 $3)}
        | '_'                                      {locatedPat $1 PWildcard}
        | Literal                                  {locatedValueWith locatedPat $1 PLit}
        | '(' Pattern ')'                          {$2}
        | PatternList                              {locatedFrom $1 locatedPat (Pat (Name "pair") $1)}

PatternList :: {[Pat]}
PatternList : '(' PatList ')'                      {$2}
            | {- empty -}                          {[]}

PatList :: { [Pat] }
PatList : Pattern %shift                           {[$1]}
        | Pattern ',' PatList                      {$1 : $3}

-- literals

Literal :: { LocatedValue Literal }
Literal : number                                   {LocatedValue (locatedValueSpan $1) (IntLit (toInteger (locatedValue $1)))}
        | stringlit                                {LocatedValue (locatedValueSpan $1) (StrLit (rmquotes (locatedValue $1)))}

-- basic type definitions

Type :: { Ty }
Type : TypeName OptTypeParam                        {locatedBetween $1 $2 locatedTy (TyCon $1 $2)}
     | LamType                                      {locatedFrom $1 locatedTy (uncurry funtype $1)}
     | TupleTy                                      {$1}
     | '@' Type                                     {locatedFrom $2 locatedTy (TyCon (Name "Proxy") [$2])}

TupleTy :: { Ty }
TupleTy : '(' TypeCommaList ')'                     {mkTupleTy $2}

LamType :: {([Ty], Ty)}
LamType : '(' TypeCommaList ')' '->' Type          {($2, $5)}

Var :: { Ty }
Var : Name                                         {locatedFrom $1 locatedTy (TyCon $1 [])}

Name :: { Name }
Name : identifier                                 { nameFromIdent $1 }

TypeName :: { Name }
TypeName : identifier                             { nameFromIdent $1 }
         | TypeName '.' identifier               { qualNameFromIdent $1 $3 }

-- Yul statments and blocks

AsmBlock :: {LocatedValue YulBlock}
AsmBlock : 'assembly' YulBlock                     {LocatedValue $1 $2}

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
YulExp : YulLiteral                                {YLit $1}
       | Name                                      {YIdent $1}
       | Name YulFunArgs                           {YCall $1 $2}
       | 'return' YulFunArgs                       {YCall (Name "return") $2}

YulFunArgs :: {[YulExp]}
YulFunArgs : '(' YulExpCommaList ')'               {$2}

YulExpCommaList :: { [YulExp] }
YulExpCommaList : YulExp                           {[$1]}
              | {- empty -}                        {[]}
              | YulExp ',' YulExpCommaList         {$1 : $3}

YulLiteral :: { YLiteral }
YulLiteral : number                                {YulNumber (toInteger (locatedValue $1))}
        | stringlit                                {YulString (rmquotes (locatedValue $1))}

OptSemi :: { () }
OptSemi : ';'                                      { () }
        | {- empty -}                              { () }

{

moduleParser :: [String] -> String -> IO (Either String CompUnit)
moduleParser _dirs = parseCompUnitWithPath "<input>"

parseCompUnitWithPath :: FilePath -> String -> IO (Either String CompUnit)
parseCompUnitWithPath sourcePath content
  = do
      let r = runAlex content (setSourceName sourcePath >> parser)
      case r of
        Left err -> pure $ Left err
        Right cunit -> pure (Right cunit)

parseCompUnit :: String -> IO (Either String CompUnit)
parseCompUnit = moduleParser []

nameFromIdent :: LocatedText -> Name
nameFromIdent ident =
  locatedName (locatedTextSpan ident) (Name (locatedTextText ident))

qualNameFromIdent :: Name -> LocatedText -> Name
qualNameFromIdent qualifier ident =
  locatedQualName qualifier (locatedTextSpan ident) (locatedTextText ident)

classifyImportPath :: Name -> ModulePath
classifyImportPath modName =
  case splitName modName of
    "lib" : rest@(_ : _) -> LibraryPath (mkName rest)
    _ -> RelativePath modName

splitName :: Name -> [String]
splitName (Name n) = [n]
splitName (QualName n s) = splitName n ++ [s]

mkName :: [String] -> Name
mkName [] = error "mkName: empty module path"
mkName (n : ns) = foldl QualName (Name n) ns

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

locatedFrom :: (HasSourceSpan anchor) => anchor -> (SourceSpan -> node -> node) -> node -> node
locatedFrom anchor locate node =
  maybe node (`locate` node) (sourceSpanOf anchor)

locatedBetween :: (HasSourceSpan left, HasSourceSpan right) => left -> right -> (SourceSpan -> node -> node) -> node -> node
locatedBetween left right locate node =
  maybe node (`locate` node) (combineMaybeSourceSpans (sourceSpanOf left) (sourceSpanOf right))

locatedValueWith :: (SourceSpan -> node -> node) -> LocatedValue value -> (value -> node) -> node
locatedValueWith locate located build =
  locate (locatedValueSpan located) (build (locatedValue located))

rmquotes :: String -> String
rmquotes = read

parseError token@(Token _ lexeme)
  = alexError $
      encodeDiagnostic
        Diagnostic
          { diagnosticSeverity = Error,
            diagnosticCode = Just (DiagnosticCode "SC0001"),
            diagnosticMessage = "parse error: unexpected " ++ lexemeDescription lexeme,
            diagnosticLabels =
              [ Label
                  { labelSpan = tokenSpan token,
                    labelStyle = Primary,
                    labelMessage = Just (labelDescription lexeme)
                  }
              ],
            diagnosticNotes = [],
            diagnosticHelp = []
          }

lexemeDescription :: Lexeme -> String
lexemeDescription TEOF = "end of file"
lexemeDescription lexeme = show lexeme

labelDescription :: Lexeme -> String
labelDescription TEOF = "expected more input"
labelDescription _ = "unexpected token"

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
