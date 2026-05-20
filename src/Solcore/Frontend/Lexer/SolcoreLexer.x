{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Solcore.Frontend.Lexer.SolcoreLexer where

import Control.Monad
import Numeric (readHex)
import Solcore.Diagnostics
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
        <0>    "export"                          {simpleToken TExport}
        <0>    "hiding"                          {simpleToken THiding}
        <0>    "as"                              {simpleToken TAs}
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
        <0>    "else"                            {simpleToken TElse}
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
        <0>    "<"                               {simpleToken TLT}
        <0>    ">"                               {simpleToken TGT}
        <0>    ">="                              {simpleToken TGE}
        <0>    "<="                              {simpleToken TLE}
        <0>    "!="                              {simpleToken TNE}
        <0>    "=="                              {simpleToken TEE}
        <0>    "&&"                              {simpleToken TLAnd}
        <0>    "||"                              {simpleToken TLOr}
        <0>    "!"                               {simpleToken TLNot}
        <0>    "+"                               {simpleToken TPlus}
        <0>    "-"                               {simpleToken TMinus}
        <0>    "*"                               {simpleToken TTimes}
        <0>    "/"                               {simpleToken TDivide}
        <0>    "%"                               {simpleToken TModulo}
        <0>    "+="                              {simpleToken TPlusEq}
        <0>    "-="                              {simpleToken TMinusEq}
        <0>    "then"                            {simpleToken TThen}
        <0>    "@"                               {simpleToken TAt}
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
    , sourceName :: FilePath
    }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState 0 (AlexPn 0 0 0) [] "<input>"

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

setSourceName :: FilePath -> Alex ()
setSourceName name =
  modify $ \s -> s{sourceName = name}

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  when (startCode == state_string) $
    alexError "Error: unclosed string"
  file <- sourceName <$> get
  pure $ mkToken file pos 0 TEOF

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

sourceSpan :: FilePath -> AlexPosn -> Int -> SourceSpan
sourceSpan file (AlexPn offset line column) len =
  SourceSpan
    { spanFile = file,
      spanStartByte = offset,
      spanEndByte = offset + len,
      spanStartLine = line,
      spanStartColumn = column,
      spanEndLine = line,
      spanEndColumn = column + max 1 len
    }

sourceSpanBetween :: FilePath -> AlexPosn -> AlexPosn -> Int -> SourceSpan
sourceSpanBetween file (AlexPn startOffset startLine startColumn) (AlexPn endOffset endLine endColumn) endLen =
  SourceSpan
    { spanFile = file,
      spanStartByte = startOffset,
      spanEndByte = endOffset + endLen,
      spanStartLine = startLine,
      spanStartColumn = startColumn,
      spanEndLine = endLine,
      spanEndColumn = endColumn + max 1 endLen
    }

mkToken :: FilePath -> AlexPosn -> Int -> Lexeme -> Token
mkToken file st len lx =
  TokenWithSpan (sourceSpan file st len) (position st) lx

mkTokenWithSpan :: SourceSpan -> Lexeme -> Token
mkTokenWithSpan span lx =
  TokenWithSpan span (spanStartLine span, spanStartColumn span) lx

-- token definition

data Token
  = TokenWithSpan {
      tokenSpan :: SourceSpan
    , pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

pattern Token :: (Int, Int) -> Lexeme -> Token
pattern Token p lx <- TokenWithSpan _ p lx
  where
    Token p lx = TokenWithSpan (uncurriedSourceSpan p) p lx

{-# COMPLETE Token #-}

uncurriedSourceSpan :: (Int, Int) -> SourceSpan
uncurriedSourceSpan (line, column) =
  SourceSpan
    { spanFile = "<input>",
      spanStartByte = 0,
      spanEndByte = 0,
      spanStartLine = line,
      spanStartColumn = column,
      spanEndLine = line,
      spanEndColumn = column + 1
    }

data Lexeme
  = TIdent { unIdent :: String }
  | TNumber { unNum :: Integer }
  | TString { unStr :: String }
  | TContract
  | TImport
  | TExport
  | THiding
  | TAs
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
  | TElse
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
  | TLT
  | TGT
  | TGE
  | TLE
  | TNE
  | TEE
  | TLAnd
  | TLOr
  | TLNot
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TModulo
  | TPlusEq
  | TMinusEq
  | TTimesEq
  | TDivideEq
  | TModuloEq
  | TNoCoverageCondition
  | TNoPattersonCondition
  | TNoBoundVariableCondition
  | TBar
  | TThen
  | TAt
  | TEOF
  deriving (Eq, Ord, Show)

-- Functions to create tokens

mkIdent :: AlexAction Token
mkIdent (st, _, _, str) len
  = do
      file <- sourceName <$> get
      pure $ mkToken file st len (lexemeFor (take len str))
  where
    lexemeFor str =
      case str of
        "match" -> TMatch
        "data" -> TData
        "import" -> TImport
        "export" -> TExport
        "hiding" -> THiding
        "as" -> TAs
        "contract" -> TContract
        "function" -> TFunction
        "constructor" -> TConstructor
        "return" -> TReturn
        "continue" -> TContinue
        "break" -> TBreak
        "let" -> TLet
        "assembly" -> TAssembly
        "if" -> TIf
        "else" -> TElse
        "switch" -> TSwitch
        "for" -> TFor
        "default" -> TDefault
        "type" -> TType
        "forall" -> TForall
        "pragma" -> TPragma
        "no-coverage-condition" -> TNoCoverageCondition
        "no-patterson-condition" -> TNoPattersonCondition
        "no-bounded-variable-condition" -> TNoBoundVariableCondition
        _ -> TIdent str

mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len
  = do
      file <- sourceName <$> get
      pure $ mkToken file st len (TNumber $ read $ take len str)

mkHexlit :: AlexAction Token
mkHexlit (st, _, _, str) len
  = do
      file <- sourceName <$> get
      pure $ mkToken file st len (TNumber $ parseHex $ take len str)

parseHex :: String -> Integer
parseHex str = case readHex (drop 2 str) of
    [(n, "")] -> n
    _         -> error "impossible :)"

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) len
  = do
      file <- sourceName <$> get
      return $ mkToken file st len lx

-- string literals

enterString :: AlexAction Token
enterString inp@(pos, _, _, _) len
  = do
      modify $ \s -> s{ strStart = pos
                      , strBuffer = '"' : strBuffer s
                      }
      skip inp len

exitString :: AlexAction Token
exitString (pos, _, _, _) len
  = do
      s <- get
      put s{strStart = AlexPn 0 0 0, strBuffer = []}
      let tk = TString $ reverse $ '"' : strBuffer s
      return $ mkTokenWithSpan (sourceSpanBetween (sourceName s) (strStart s) pos len) tk

emit :: Char -> AlexAction Token
emit c inp@(_, _, _, _) len = do
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
