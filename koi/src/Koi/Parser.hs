{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Koi.Parser
  ( parseProgram
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Array.IArray
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Void
import Koi.Board
import Koi.Program
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data PExpr
  = PLit Integer
  | PDef Text
  | PPtr PPointer
  | PAdd ParsedExpr ParsedExpr
  | PSub ParsedExpr ParsedExpr

data PPointer = PPointer ParsedExpr ParsedExpr ParsedExpr ParsedExpr ParsedExpr

type ParsedExpr = (Int, PExpr)

data ParserState = ParserState { commandIndex :: Integer, size :: Maybe (Integer, Integer), definitions :: Map Text ParsedExpr }

defaultState :: ParserState
defaultState = ParserState { commandIndex = 0, size = Nothing, definitions = M.empty }

type Parser = ParsecT Void Text (State ParserState)

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) Program
parseProgram fileName code = evalState (runParserT program fileName code) defaultState

program :: Parser Program
program = do
  skipSpace
  unresolvedStatements <- catMaybes <$> many statement
  eof <|> void statement -- The statement should never be parsed, but it improves the error message
  statements <- sequence unresolvedStatements
  s <- get
  pure Program { programSize = fromMaybe (256, 256) $ size s, programCode = listToArray statements }

statement :: Parser (Maybe (Parser Command))
statement = Nothing <$ pragma <|> Just <$> command

pragma :: Parser ()
pragma = sizePragma <|> definePragma <|> labelPragma

sizePragma :: Parser ()
sizePragma = do
  pos <- getOffset
  word "_size"
  s <- get
  if commandIndex s /= 0 then
    failAt pos "_size cannot be specified after the first command"
  else if isJust $ size s then
    failAt pos "_size can only be specified once"
  else parens $ do
    width <- positiveNumber
    symbol ","
    height <- positiveNumber
    put s { size = Just (width, height) }

definePragma :: Parser ()
definePragma = do
  word "_define"
  parens $ do
    pos <- getOffset
    name <- identifier
    symbol ","
    val <- expr
    define name val pos

labelPragma :: Parser ()
labelPragma = do
  word "_label"
  parens $ do
    pos <- getOffset
    name <- identifier
    i <- gets $ (pos, ) . PLit . commandIndex
    define name i pos

define :: Text -> ParsedExpr -> Int -> Parser ()
define name val pos = do
  s <- get
  if M.member name $ definitions s then
      failAt pos $ unpack name ++ " is already defined"
    else
      put $ s { definitions = M.insert name val $ definitions s }

command :: Parser (Parser Command)
command = do
  cmd <- gotoCommand <|> passCommand <|> playCommand "black" (Play Black) <|> playCommand "white" (Play White) <|> ifCommand <|> copyCommand
  s <- get
  put s { commandIndex = commandIndex s + 1 }
  pure cmd

gotoCommand :: Parser (Parser Command)
gotoCommand = do
  word "goto"
  parens $ do
    location <- pexpr
    pure $ Goto <$> location

passCommand :: Parser (Parser Command)
passCommand = do
  word "pass"
  parens . pure $ pure Pass

playCommand :: Text -> (PtrExpr -> Expr -> Command) -> Parser (Parser Command)
playCommand player f = do
  word player
  parens $ do
    ptr <- pptr
    val <- fromMaybe (pure $ Lit (-1)) <$> optional (symbol "," *> pexpr)
    pure $ f <$> ptr <*> val

ifCommand :: Parser (Parser Command)
ifCommand = do
  word "if"
  parens $ do
    condition <- pptr
    symbol ","
    location <- pexpr
    pure $ If <$> condition <*> location

copyCommand :: Parser (Parser Command)
copyCommand = do
  word "copy"
  parens $ do
    to <- pptr
    symbol ","
    from <- pptr
    pure $ Copy <$> to <*> from

pexpr :: Parser (Parser Expr)
pexpr = resolveExpr <$> expr

pptr :: Parser (Parser PtrExpr)
pptr = resolvePtr <$> expr

expr :: Parser ParsedExpr
expr = foldl (\e (f, e') -> (fst e, f e e')) <$> value <*> many addSubValue

addSubValue :: Parser (ParsedExpr -> ParsedExpr -> PExpr, ParsedExpr)
addSubValue = (,) <$> operator <*> value

value :: Parser ParsedExpr
value = (,) <$> getOffset <*> (numberValue <|> definitionValue <|> pointerValue)

numberValue :: Parser PExpr
numberValue = PLit <$> number

definitionValue :: Parser PExpr
definitionValue = PDef <$> identifier

pointerValue :: Parser PExpr
pointerValue = PPtr <$> pointer

pointer :: Parser PPointer
pointer = cellPointer <|> vectorPointer

cellPointer :: Parser PPointer
cellPointer = do
  symbol "["
  x <- expr
  symbol ","
  y <- expr
  symbol "]"
  pure $ PPointer (0, PLit 1) x y (0, PLit 0) (0, PLit 0)

vectorPointer :: Parser PPointer
vectorPointer = do
  symbol "<"
  b <- expr
  symbol ","
  x <- expr
  symbol ","
  y <- expr
  symbol ","
  dx <- expr
  symbol ","
  dy <- expr
  symbol ">"
  pure $ PPointer b x y dx dy

operator :: Parser (ParsedExpr -> ParsedExpr -> PExpr)
operator = symbol "+" $> PAdd <|> symbol "-" $> PSub

resolveExpr :: ParsedExpr -> Parser Expr
resolveExpr (_, PLit x) = pure $ Lit x
resolveExpr (pos, PDef name) = resolveExpr =<< resolveName name pos
resolveExpr p@(_, PPtr _) = Ptr <$> resolvePtr p
resolveExpr (_, PAdd a b) = Ptr <$> (Add <$> resolvePtr a <*> resolvePtr b)
resolveExpr (_, PSub a b) = Ptr <$> (Sub <$> resolvePtr a <*> resolvePtr b)

resolvePtr :: ParsedExpr -> Parser PtrExpr
resolvePtr (pos, PLit _) = failAt pos "Pointer expected"
resolvePtr (pos, PDef name) = do
  (_, ptr) <- resolveName name pos
  resolvePtr (pos, ptr)
resolvePtr (_, PPtr (PPointer b x y dx dy)) = PtrLit <$> (Pointer <$> resolveExpr b <*> resolveExpr x <*> resolveExpr y <*> resolveExpr dx <*> resolveExpr dy)
resolvePtr (_, PAdd a b) = Add <$> resolvePtr a <*> resolvePtr b
resolvePtr (_, PSub a b) = Sub <$> resolvePtr a <*> resolvePtr b

resolveName :: Text -> Int -> Parser ParsedExpr
resolveName name pos = do
  defs <- gets definitions
  case M.lookup name defs of
    Nothing -> failAt pos $ unpack name ++ " is not defined"
    Just e -> pure e

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

word :: Text -> Parser ()
word s = lexeme $ do
  pos <- getOffset
  region (const . TrivialError pos Nothing . S.singleton . Tokens . NE.fromList . unpack $ s) . try $ do
    void $ chunk s
    notFollowedBy identifierChar

number :: Parser Integer
number = do
  negative <- isJust <$> optional (symbol "-")
  (if negative then negate else id) <$> positiveNumber

positiveNumber :: Parser Integer
positiveNumber = decimal <|> hexadecimal <|> octal <|> binary

decimal :: Parser Integer
decimal = try . lexeme $ L.decimal <* notFollowedBy identifierChar

hexadecimal :: Parser Integer
hexadecimal = lexeme $ do
  void $ chunk "0x"
  L.hexadecimal <* notFollowedBy identifierChar

octal :: Parser Integer
octal = lexeme $ do
  void $ chunk "0o"
  L.octal <* notFollowedBy identifierChar

binary :: Parser Integer
binary = lexeme $ do
  void $ chunk "0b"
  L.binary <* notFollowedBy identifierChar

identifier :: Parser Text
identifier = lexeme $ pack <$> ((:) <$> identifierStartChar <*> many identifierChar)

identifierStartChar :: Parser Char
identifierStartChar = letterChar <|> char '_'

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Text -> Parser ()
symbol s = void $ L.symbol skipSpace s

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment "#") empty

failAt :: Int -> String -> Parser a
failAt pos msg = parseError . FancyError pos . S.singleton $ ErrorFail msg

listToArray :: [a] -> Array Integer a
listToArray xs = listArray (0, toInteger $ length xs - 1) xs