{
module Solcore.Frontend.Parser.SolverInputParser where

import Data.Either
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
      'reduce'   {Token _ TReduce}
      'class'    {Token _ TClass}
      'instance' {Token _ TInstance}
      'forall'   {Token _ TForall}
      ';'        {Token _ TSemi}
      ':'        {Token _ TColon}
      '~'        {Token _ TEquiv}
      ','        {Token _ TComma}
      '=>'       {Token _ TDArrow}
      '('        {Token _ TLParen}
      ')'        {Token _ TRParen}
      '{'        {Token _ TLBrace}
      '}'        {Token _ TRBrace}

%expect 0

%%
-- input definition 

Input :: { SolverState }
Input :  TopDeclList ToSolve { SolverState (uncurry Theta (partitionEithers $1)) $2 } 

TopDeclList :: { [Either (Qual Pred) (Qual Pred)] }
TopDeclList : TopDecl TopDeclList                  { $1 : $2 }
             | {- empty -}                         { [] }

ToSolve :: { SolverProblem }
ToSolve : 'sat' ':' '{' ConstraintList '}' ';'     {Sat $4}
        | 'reduce' ':' '{' ConstraintList '}' '~' '{' ConstraintList '}' ';'  {Reduce $4 $8}

-- top level declarations 

TopDecl :: { Either (Qual Pred) (Qual Pred) }
TopDecl : ClassDef                                 {Left $1}
        | InstDef                                  {Right $1}

-- class definitions 

ClassDef :: { Qual Pred }
ClassDef 
  : ClassPrefix 'class' Var ':' Name OptParam ';' {$1 :=> (InCls $5 $3 $6)}

ClassPrefix :: { [Pred] }
ClassPrefix : {- empty -}                      {[]}
           | 'forall' ConstraintList '.'       {$2}



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
               |                                   {[]}

Constraint :: { Pred }
Constraint : Type ':' Name OptTypeParam             {InCls $3 $1 $4} 

-- instance declarations 

InstDef :: { Qual Pred }
InstDef : InstPrefix 'instance' Type ':' Name OptTypeParam ';' { $1 :=> (InCls $5 $3 $6) }

InstPrefix :: { [Pred] }
InstPrefix : {- empty -}                      {[]}
           | 'forall' ConstraintList '.'      {$2}

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
data Theta 
  = Theta {
      classes :: [Qual Pred]
    , insts :: [Qual Pred]
    } deriving Show

data SolverState 
  = SolverState {
      theta :: Theta
    , problem :: SolverProblem 
    } deriving Show 

data SolverProblem 
  = Sat [Pred] 
  | Reduce [Pred] [Pred]
  | Improvement Scheme 
  deriving Show 

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

runParser :: String -> Either String SolverState 
runParser content = do
  runAlex content parser
}
