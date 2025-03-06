{
module Solcore.Frontend.Parser.SolverInputParser where

import Data.List.NonEmpty (NonEmpty, cons, singleton)

import Solcore.Frontend.Lexer.SolverInputLexer hiding (lexer)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives hiding (pairTy)
import Language.Yul
}


%name parser Input
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}

%token
      identifier {Token _ (TIdent $$)}
      '.'        {Token _ TDot}
      'sat'      {Token _ TSat}
      'class'    {Token _ TClass}
      'instance' {Token _ TInstance}
      ';'        {Token _ TSemi}
      ':'        {Token _ TColon}
      ','        {Token _ TComma}
      '=>'       {Token _ TDArrow}
      '('        {Token _ TLParen}
      ')'        {Token _ TRParen}

%expect 0

%%
-- input definition 

Input :: { ([Qual Pred], [Pred]) }
Input :  TopDeclList ToSolve { ($1, $2) } 

TopDeclList :: { [Qual Pred] }
TopDeclList : TopDecl TopDeclList                  { $1 : $2 }
             | {- empty -}                         { [] }

ToSolve :: { [Pred] }
ToSolve : 'sat' ':' ConstraintList ';'             {$3}

-- top level declarations 

TopDecl :: { Qual Pred }
TopDecl : ClassDef                                 {$1}
        | InstDef                                  {$1}

-- class definitions 

ClassDef :: { Qual Pred }
ClassDef 
  : 'class' ContextOpt Var ':' Name OptParam ';' {$2 :=> (InCls $5 $3 $6)}

OptParam :: { [Ty] }
OptParam :  '(' VarCommaList ')'                   {$2}
         | {- empty -}                             {[]}

VarCommaList :: { [Ty] }
VarCommaList : Var ',' VarCommaList                {$1 : $3} 
             | Var                                 {[$1]}

ContextOpt :: {[Pred]}
ContextOpt : {- empty -} %shift                    {[]}
           | Context                               {$1}

Context :: {[Pred]}
Context : '(' ConstraintList ')' '=>'              { $2 }   

ConstraintList :: { [Pred] }
ConstraintList : Constraint ',' ConstraintList     {$1 : $3}
               | Constraint                        {[$1]}

Constraint :: { Pred }
Constraint : Type ':' Name OptTypeParam             {InCls $3 $1 $4} 

-- instance declarations 

InstDef :: { Qual Pred }
InstDef : 'instance' ContextOpt Type ':' Name OptTypeParam ';' { $2 :=> (InCls $5 $3 $6)}

OptTypeParam :: { [Ty] }
OptTypeParam : '(' TypeCommaList ')'          {$2}
             | {- empty -}                    {[]}

TypeCommaList :: { [Ty] }
TypeCommaList : Type ',' TypeCommaList             {$1 : $3}
              | Type                               {[$1]}
              | {- empty -}                        { [] }

-- basic type definitions 

Type :: { Ty }
Type : Name OptTypeParam                            {TyCon $1 $2}
     | TupleTy                                      {$1}

TupleTy :: { Ty }
TupleTy : '(' TypeCommaList ')'                     {mkTupleTy $2}

Var :: { Ty }
Var : Name                                         {TyCon $1 []}  

Name :: { Name }  
Name : identifier                               { Name $1 }
     | QualName %shift                          { QualName (fst $1) (snd $1) }

QualName :: { (Name, String) }
QualName : QualName '.' identifier              { (QualName (fst $1) (snd $1), $3)}

{
pairTy :: Ty -> Ty -> Ty 
pairTy t1 t2 = TyCon "pair" [t1,t2]

mkTupleTy :: [Ty] -> Ty 
mkTupleTy [] = TyCon (Name "()") []
mkTupleTy ts = foldr1 pairTy ts 

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

runParser :: String -> Either String ([Qual Pred], [Pred])
runParser content = do
  runAlex content parser
}
