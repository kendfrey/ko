{-# LANGUAGE TupleSections #-}

module Koi.Parser
  ( readProgram
  ) where

import Control.Monad
import Data.Array.IArray
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Koi.Lexer
import Koi.Program
import Numeric
import Text.Parsec
import Text.Parsec.String

readProgram :: String -> IO (Either ParseError Program)
readProgram file = parseFromFile parseProgram file

parseProgram :: Parser Program
parseProgram = do
  toks <- lexer
  case runParser program (ParserState { size = Nothing, definitions = M.empty, inPrelude = True }) "" toks of
    Right result -> pure result
    Left err -> fail $ show err

data ParserState = ParserState { size :: Maybe (Int, Int), definitions :: Map String Expr, inPrelude :: Bool }

program :: Parsec [Token] ParserState Program
program = do
  statements <- catMaybes <$> endBy statement (symbolToken Semicolon)
  eof
  s <- size <$> getState
  case buildLabels (fst <$> statements) of
    Right m -> pure Program { programSize = fromMaybe (256, 256) s, programLabels = m, programCode = listToArray (snd <$> statements) }
    Left (p, l) -> do
      fail $ l ++ " is already defined"

buildLabels :: [Maybe (SourcePos, String)] -> Either (SourcePos, String) (Map String Int)
buildLabels = buildLabelsImpl 0 M.empty

buildLabelsImpl :: Int -> Map String Int -> [Maybe (SourcePos, String)] -> Either (SourcePos, String) (Map String Int)
buildLabelsImpl _ m [] = Right m
buildLabelsImpl x m (Nothing : cs) = buildLabelsImpl (x + 1) m cs
buildLabelsImpl x m (Just (p, l) : cs) =
  if M.member l m then
    Left (p, l)
  else
    buildLabelsImpl (x + 1) (M.insert l x m) cs

statement :: Parsec [Token] ParserState (Maybe (Maybe (SourcePos, String), Command))
statement = Nothing <$ pragma <|> Just <$> command

pragma :: Parsec [Token] ParserState ()
pragma = sizePragma <|> definitionPragma

sizePragma :: Parsec [Token] ParserState ()
sizePragma = do
  wordToken "_size"
  width <- numberToken
  height <- numberToken
  state <- getState
  if not (inPrelude state) then
    fail "_size must be specified before the first command in the file"
  else if isJust (size state) then
    fail "_size cannot be specified more than once"
  else
    void . putState $ state { size = Just (width, height) }

definitionPragma :: Parsec [Token] ParserState ()
definitionPragma = do
  wordToken "_define"
  name <- identifierToken
  val <- expr
  s <- getState
  if M.member name (definitions s) then
    fail $ name ++ " is already defined"
  else
    putState $ s { definitions = M.insert name val (definitions s) }

command :: Parsec [Token] ParserState (Maybe (SourcePos, String), Command)
command = do
  p <- getPosition
  l <- optionMaybe lbl
  c <- commandBody
  s <- getState
  putState $ s { inPrelude = False }
  pure ((p,) <$> l, c)

lbl :: Parsec [Token] ParserState String
lbl = do
  symbolToken Colon
  identifierToken

commandBody :: Parsec [Token] ParserState Command
commandBody = gotoCommand <|> passCommand <|> playCommand "black" PlayBlack <|> playCommand "white" PlayWhite <|> ifCommand <|> tableCommand <|> copyCommand

gotoCommand :: Parsec [Token] ParserState Command
gotoCommand = do
  wordToken "goto"
  Goto <$> identifierToken

passCommand :: Parsec [Token] ParserState Command
passCommand = wordToken "pass" $> Pass

playCommand :: String -> (Pointer -> Command) -> Parsec [Token] ParserState Command
playCommand player c = do
  wordToken player
  ptr <- toPointer <$> value
  case ptr of
    Just p -> pure $ c p
    Nothing -> fail "expected pointer"

ifCommand :: Parsec [Token] ParserState Command
ifCommand = do
  wordToken "if"
  e <- expr
  l <- identifierToken
  pure $ If e l

tableCommand :: Parsec [Token] ParserState Command
tableCommand = do
  wordToken "table"
  e <- expr
  l <- many identifierToken
  pure $ Table e (listToArray l)

copyCommand :: Parsec [Token] ParserState Command
copyCommand = do
  wordToken "copy"
  from <- toPointer <$> value
  to <- toPointer <$> value
  case (,) <$> from <*> to of
    Just (f, t) -> pure $ Copy f t
    Nothing -> fail "expected pointer"

expr :: Parsec [Token] ParserState Expr
expr = chainl1 value operator

operator :: Parsec [Token] ParserState (Expr -> Expr -> Expr)
operator = symbolToken Plus $> EAdd <|> symbolToken Minus $> ESub

value :: Parsec [Token] ParserState Expr
value = numberValue <|> definitionValue <|> pointerValue

numberValue :: Parsec [Token] ParserState Expr
numberValue = ELit <$> numberToken

definitionValue :: Parsec [Token] ParserState Expr
definitionValue = do
  name <- identifierToken
  ParserState _ d _ <- getState
  case M.lookup name d of
    Just e -> pure e
    Nothing -> fail $ name ++ " is not defined"

pointerValue :: Parsec [Token] ParserState Expr
pointerValue = EPtr <$> (cellPointer <|> vectorPointer)

cellPointer :: Parsec [Token] ParserState Pointer
cellPointer = do
  symbolToken LBracket
  x <- expr
  symbolToken Comma
  y <- expr
  symbolToken RBracket
  pure $ Pointer (ELit 1) x y (ELit 0) (ELit 0)

vectorPointer :: Parsec [Token] ParserState Pointer
vectorPointer = do
  symbolToken LAngle
  b <- expr
  symbolToken Comma
  x <- expr
  symbolToken Comma
  y <- expr
  symbolToken Comma
  dx <- expr
  symbolToken Comma
  dy <- expr
  symbolToken RAngle
  pure $ Pointer b x y dx dy

identifierToken :: Parsec [Token] ParserState String
identifierToken = koiToken ident
  where
    ident (Token s Identifier _) = Just s
    ident _ = Nothing

wordToken :: String -> Parsec [Token] ParserState ()
wordToken name = koiToken ident
  where
    ident (Token s Identifier _) | s == name = Just ()
    ident _ = Nothing

symbolToken :: TokenType -> Parsec [Token] ParserState ()
symbolToken tokType = koiToken symb
  where
    symb (Token _ t _) | t == tokType = Just ()
    symb _ = Nothing

numberToken :: Parsec [Token] ParserState Int
numberToken = koiToken num
  where
    num (Token s Decimal _) = Just . fst . head $ readDec s
    num (Token ('0':'x':s) Hex _) = Just . fst . head $ readHex s
    num (Token ('0':'o':s) Octal _) = Just . fst . head $ readOct s
    num (Token ('0':'b':s) Binary _) = Just . fst . head $ readInt 2 (`elem` "01") (fromJust . (`elemIndex` "01")) s
    num _ = Nothing

koiToken :: (Token -> Maybe a) -> Parsec [Token] u a
koiToken = token tokenValue tokenPos

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs