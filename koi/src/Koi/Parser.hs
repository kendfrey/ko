{-# LANGUAGE TupleSections #-}

module Koi.Parser
  ( parseProgram
  ) where

import Control.Monad.State
import Data.Array.IArray
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Koi.Board
import Koi.Program
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ParserState = ParserState { size :: Maybe (Int, Int), definitions :: Map String Expr, labels :: Set String, inPrelude :: Bool }

defaultState :: ParserState
defaultState = ParserState { size = Nothing, definitions = M.empty, labels = S.empty, inPrelude = True }

type Parser = ParsecT Void String (State ParserState)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProgram fileName code = evalState (runParserT program fileName code) defaultState

program :: Parser Program
program = do
  skipSpace
  statements <- catMaybes <$> many (statement <* symbol ";")
  eof <|> void statement -- The statement should never be parsed, but it improves the error message
  s <- get
  pure Program { programSize = fromMaybe (256, 256) $ size s, programLabels = M.fromList . makeLabels $ fst <$> statements, programCode = listToArray (snd <$> statements) }

makeLabels :: [Maybe String] -> [(String, Int)]
makeLabels ls = catMaybes . map (\(s, i) -> (, i) <$> s) $ zip ls [0..]

statement :: Parser (Maybe (Maybe String, Command))
statement = Nothing <$ pragma <|> Just <$> labeledCommand

pragma :: Parser ()
pragma = sizePragma <|> definePragma

sizePragma :: Parser ()
sizePragma = do
  pos <- getOffset
  word "_size"
  s <- get
  if not $ inPrelude s then
    failAt pos "_size must be specified before the first command"
  else if isJust $ size s then
    failAt pos "_size can only be specified once"
  else do
    width <- number
    height <- number
    put s { size = Just (width, height) }

definePragma :: Parser ()
definePragma = do
  word "_define"
  pos <- getOffset
  name <- identifier
  s <- get
  if M.member name (definitions s) then
    failAt pos $ name ++ " is already defined"
  else do
    val <- expr
    put $ s { definitions = M.insert name val $ definitions s }

labeledCommand :: Parser (Maybe String, Command)
labeledCommand = do
  lbl <- optional label
  cmd <- command
  s <- get
  put s { inPrelude = False }
  pure (lbl, cmd)

label :: Parser String
label = do
  symbol ":"
  pos <- getOffset
  name <- identifier
  s <- get
  if S.member name $ labels s then
    failAt pos $ name ++ " is already defined"
  else do
    put s { labels = S.insert name $ labels s }
    pure name

command :: Parser Command
command = gotoCommand <|> passCommand <|> playCommand "black" (Play Black) <|> playCommand "white" (Play White) <|> ifCommand <|> caseCommand <|> copyCommand

gotoCommand :: Parser Command
gotoCommand = do
  word "goto"
  Goto <$> identifier

passCommand :: Parser Command
passCommand = do
  word "pass"
  pure Pass

playCommand :: String -> (Pointer -> Command) -> Parser Command
playCommand player f = do
  word player
  pos <- getOffset
  ptr <- toPointer <$> expr
  case ptr of
    Just p -> pure $ f p
    Nothing -> failAt pos "pointer required"

ifCommand :: Parser Command
ifCommand = do
  word "if"
  If <$> expr <*> identifier

caseCommand :: Parser Command
caseCommand = do
  word "case"
  Case <$> expr <*> (listToArray <$> many identifier)

copyCommand :: Parser Command
copyCommand = do
  word "copy"
  posFrom <- getOffset
  from <- toPointer <$> expr
  case from of
    Just f -> do
      posTo <- getOffset
      to <- toPointer <$> expr
      case to of
        Just t -> pure $ Copy f t
        Nothing -> failAt posTo "pointer required"
    Nothing -> failAt posFrom "pointer required"

expr :: Parser Expr
expr = do
  first <- value
  rest <- many addSubValue
  pure $ foldl (\e (f, e') -> f e e') first rest

addSubValue :: Parser (Expr -> Expr -> Expr, Expr)
addSubValue = do
  op <- operator
  val <- value
  pure (op, val)

value :: Parser Expr
value = numberValue <|> definitionValue <|> pointerValue

numberValue :: Parser Expr
numberValue = ELit <$> number

definitionValue :: Parser Expr
definitionValue = do
  pos <- getOffset
  name <- identifier
  d <- gets definitions
  case M.lookup name d of
    Just e -> pure e
    Nothing -> failAt pos $ name ++ " is not defined"

pointerValue :: Parser Expr
pointerValue = EPtr <$> (cellPointer <|> vectorPointer)

cellPointer :: Parser Pointer
cellPointer = do
  symbol "["
  x <- expr
  symbol ","
  y <- expr
  symbol "]"
  pure $ Pointer (ELit 1) x y (ELit 0) (ELit 0)

vectorPointer :: Parser Pointer
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
  pure $ Pointer b x y dx dy

operator :: Parser (Expr -> Expr -> Expr)
operator = symbol "+" $> EAdd <|> symbol "-" $> ESub

word :: String -> Parser ()
word s = lexeme $ do
  pos <- getOffset
  region (const . TrivialError pos Nothing . S.singleton . Tokens $ NE.fromList s) . try $ do
    void $ chunk s
    notFollowedBy identifierChar

number :: Parser Int
number = try $ decimal <|> hexadecimal <|> octal <|> binary

decimal :: Parser Int
decimal = try . lexeme $ L.decimal <* notFollowedBy identifierChar

hexadecimal :: Parser Int
hexadecimal = lexeme $ do
  void $ chunk "0x"
  L.hexadecimal <* notFollowedBy identifierChar

octal :: Parser Int
octal = lexeme $ do
  void $ chunk "0o"
  L.octal <* notFollowedBy identifierChar

binary :: Parser Int
binary = lexeme $ do
  void $ chunk "0b"
  L.binary <* notFollowedBy identifierChar

identifier :: Parser String
identifier = lexeme $ (:) <$> identifierStartChar <*> many identifierChar

identifierStartChar :: Parser Char
identifierStartChar = letterChar <|> char '_'

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: String -> Parser ()
symbol s = void $ L.symbol skipSpace s

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment "#") empty

failAt :: Int -> String -> Parser a
failAt pos msg = parseError . FancyError pos . S.singleton $ ErrorFail msg

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs