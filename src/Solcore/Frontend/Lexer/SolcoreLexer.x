{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Solcore.Frontend.Lexer.SolcoreLexer where

import Control.Monad
import Numeric (readHex)
}


%wrapper "monadUserState"

$digit = 0-9      -- digits
$lower = [a-z]    -- lower case chars
$upper = [A-Z]    -- upper case chars
$special = [\_]   -- special characters
$alpha = [a-zA-Z] -- alphabetic characters
$hexdig = [0-9A-Fa-f]

-- second RE macros

@identifier = $alpha[$alpha $special $digit]* -- identifiers
@number     = $digit+
@hexlit     = 0x$hexdig+

-- tokens declarations

tokens :-
        -- whitespace and comments 
        
        <0>    $white+                           ;
        <0>    "//" .*                           ;
        <0>   "/*"                               {nestComment `andBegin` state_comment}
        <0>   "*/"                               {\ _ _ -> alexError "Error: unexpected close comment!"}
        <state_comment> "/*"                     {nestComment}
        <state_comment> "*/"                     {unnestComment}
        <state_comment> .                        ;  
        <state_comment> \n                       ;

        -- keywords, and operators

        <0>    "contract"                        {simpleToken TContract}
        <0>    "import"                          {simpleToken TImport}
        <0>    "let"                             {simpleToken TLet}
        <0>    "data"                            {simpleToken TData}
        <0>    "."                               {simpleToken TDot}
        <0>    ":"                               {simpleToken TColon}
        <0>    "="                               {simpleToken TEq}
        <0>    ":="                              {simpleToken TYAssign}
        <0>    "class"                           {simpleToken TClass}
        <0>    "forall"                          {simpleToken TForall}
        <0>    "instance"                        {simpleToken TInstance}
        <0>    "if"                              {simpleToken TIf}
        <0>    "for"                             {simpleToken TFor}
        <0>    "switch"                          {simpleToken TSwitch}
        <0>    "type"                            {simpleToken TType}
        <0>    "case"                            {simpleToken TCase}
        <0>    "default"                         {simpleToken TDefault}
        <0>    "match"                           {simpleToken TMatch}
        <0>    "function"                        {simpleToken TFunction}
        <0>    "constructor"                     {simpleToken TConstructor}
        <0>    "return"                          {simpleToken TReturn}
        <0>    "leave"                           {simpleToken TLeave}
        <0>    "continue"                        {simpleToken TContinue}
        <0>    "break"                           {simpleToken TBreak}
        <0>    "lam"                             {simpleToken TLam}
        <0>    "assembly"                        {simpleToken TAssembly}
        <0>    "pragma"                          {simpleToken TPragma}
        <0>    "no-coverage-condition"           {simpleToken TNoCoverageCondition}
        <0>    "no-patterson-condition"          {simpleToken TNoPattersonCondition}
        <0>    "no-bounded-variable-condition"     {simpleToken TNoBoundVariableCondition}
        <0>    "bool-type-decl"                  {simpleToken TBoolType}
        <0>    "->"                              {simpleToken TArrow}
        <0>    "=>"                              {simpleToken TDArrow}
        <0>    ";"                               {simpleToken TSemi}
        <0>    "_"                               {simpleToken TWildCard}
        <0>    "("                               {simpleToken TLParen}
        <0>    ")"                               {simpleToken TRParen}
        <0>    "{"                               {simpleToken TLBrace}
        <0>    "}"                               {simpleToken TRBrace}
        <0>    ","                               {simpleToken TComma}
        <0>    "["                               {simpleToken TLBrack}
        <0>    "]"                               {simpleToken TRBrack}
        <0>    "|"                               {simpleToken TBar}
        <0>    @identifier                       {mkIdent}
        <0>    @number                           {mkNumber}
        <0>    @hexlit                           {mkHexlit}

        -- string literals 

        <0> \"                                   {enterString `andBegin` state_string}
        <state_string> \\n                       {emit '\n'}
        <state_string> \\t                       {emit '\t'}
        <state_string>  \\\"                     {emit '\"'}
        <state_string>  \"                       {exitString `andBegin` 0}
        <state_string>  .                        {emitCurrent}

{
-- user state 

data AlexUserState 
  = AlexUserState {
      nestLevel :: Int 
    , strStart :: AlexPosn
    , strBuffer :: String 
    }

alexInitUserState :: AlexUserState 
alexInitUserState 
  = AlexUserState 0 (AlexPn 0 0 0) []

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f 
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  when (startCode == state_string) $
    alexError "Error: unclosed string"
  pure $ Token (position pos) TEOF

-- FIXME: Use AlexPosn in the token type to represent the location.

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TIdent { unIdent :: String }
  | TNumber { unNum :: Integer }
  | TString { unStr :: String }
  | TContract 
  | TImport 
  | TLet
  | TEq 
  | TDot
  | TColon
  | TComma
  | TForall 
  | TClass 
  | TInstance 
  | TData 
  | TMatch
  | TIf 
  | TFor 
  | TSwitch 
  | TType
  | TCase 
  | TDefault
  | TContinue 
  | TLeave 
  | TBreak 
  | TFunction
  | TConstructor
  | TReturn 
  | TLam
  | TYAssign
  | TAssembly
  | TSemi
  | TWildCard
  | TArrow
  | TDArrow
  | TLParen 
  | TRParen
  | TLBrace
  | TRBrace
  | TPragma
  | TLBrack
  | TRBrack
  | TNoCoverageCondition
  | TNoPattersonCondition
  | TNoBoundVariableCondition
  | TBoolType
  | TBar 
  | TEOF 
  deriving (Eq, Ord, Show)

-- Functions to create tokens 

mkIdent :: AlexAction Token 
mkIdent (st, _, _, str) len 
  = case take len str of 
      "match" -> return $ Token (position st) TMatch
      "data" -> return $ Token (position st) TData 
      "import" -> return $ Token (position st) TImport 
      "contract" -> return $ Token (position st) TContract
      "function" -> return $ Token (position st) TFunction
      "constructor" -> return $ Token (position st) TConstructor
      "return" -> return $ Token (position st) TReturn
      "continue" -> return $ Token (position st) TContinue 
      "break" -> return $ Token (position st) TBreak 
      "let" -> return $ Token (position st) TLet
      "assembly" -> return $ Token (position st) TAssembly
      "if" -> return $ Token (position st) TIf 
      "switch" -> return $ Token (position st) TSwitch 
      "for" -> return $ Token (position st) TFor 
      "default" -> return $ Token (position st) TDefault
      "type" -> return $ Token (position st) TType
      "forall" -> return $ Token (position st) TForall
      "pragma" -> return $ Token (position st) TPragma
      "no-coverage-condition" -> 
        return $ Token (position st) TNoCoverageCondition
      "no-patterson-condition" -> 
        return $ Token (position st) TNoPattersonCondition
      "no-bounded-variable-condition" -> 
        return $ Token (position st) TNoBoundVariableCondition
      "bool-type-decl" -> 
        return $ Token (position st) TBoolType
      _ -> return $ Token (position st) (TIdent $ take len str)

mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len 
  = pure $ Token (position st) (TNumber $ read $ take len str)

mkHexlit :: AlexAction Token
mkHexlit (st, _, _, str) len
  = pure $ Token (position st) (TNumber $ parseHex $ take len str)

parseHex :: String -> Integer 
parseHex str = case readHex (drop 2 str) of
    [(n, "")] -> n
    _         -> error "impossible :)"

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) len
  = return $ Token (position st) lx

-- string literals 

enterString :: AlexAction Token
enterString inp@(pos, _, _, _) len 
  = do
      modify $ \s -> s{ strStart = pos
                      , strBuffer = '"' : strBuffer s
                      }
      skip inp len

exitString :: AlexAction Token 
exitString inp@(pos, _, _, _) len 
  = do
      s <- get
      put s{strStart = AlexPn 0 0 0, strBuffer = []}
      let tk = TString $ reverse $ '"' : strBuffer s
      return $ Token (position pos) tk

emit :: Char -> AlexAction Token
emit c inp@(_, _, _, str) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

emitCurrent :: AlexAction Token
emitCurrent (_, _, _, []) _ = alexError "Error: Expecting EOF!"
emitCurrent inp@(_, _, _, (c : _)) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token 
unnestComment input len 
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len


lexer :: String -> Either String [Token]
lexer s = runAlex s go 
  where 
    go = do 
      output <- alexMonadScan 
      if lexeme output == TEOF then 
        pure [output]
      else (output :) <$> go
}
