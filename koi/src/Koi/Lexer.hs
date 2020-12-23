{-# LANGUAGE TupleSections #-}

module Koi.Lexer where

import Control.Monad
import Text.Parsec
import Text.Parsec.String

data TokenType
  = Identifier
  | Decimal
  | Hex
  | Octal
  | Binary
  | Plus
  | Minus
  | Comma
  | Colon
  | Semicolon
  | LBracket
  | RBracket
  | LAngle
  | RAngle
  deriving Show

data Token = TokenInfo { tokenValue :: String, tokenType :: TokenType, tokenPos :: SourcePos }
  deriving Show

lexer :: Parser [Token]
lexer = do
  ignored
  many parseToken <* eof

ignored :: Parser ()
ignored = skipMany (void space <|> comment)

comment :: Parser ()
comment = do
  void $ char '#'
  void . many $ noneOf "\r\n"
  (void endOfLine <|> eof)
  pure ()

parseToken :: Parser Token
parseToken = do
  p <- getPosition
  (s, t) <- getToken
  ignored
  pure (TokenInfo s t p)

getToken :: Parser (String, TokenType)
getToken = identifier <|> decimal <|> hex <|> octal <|> binary <|> choice (symbol <$> symbols)

identifier :: Parser (String, TokenType)
identifier = do
  first <- char '_' <|> letter
  rest <- many (char '_' <|> alphaNum)
  pure (first : rest, Identifier)

decimal :: Parser (String, TokenType)
decimal = try $ (, Decimal) <$> many1 digit <* notFollowedBy (char '_' <|> alphaNum)

hex :: Parser (String, TokenType)
hex = try $ (, Hex) <$> string "0x" <> many1 hexDigit <* notFollowedBy (char '_' <|> alphaNum)

octal :: Parser (String, TokenType)
octal = try $ (, Octal) <$> string "0o" <> many1 octDigit <* notFollowedBy (char '_' <|> alphaNum)

binary :: Parser (String, TokenType)
binary = try $ (, Binary) <$> string "0b" <> many1 (oneOf "01") <* notFollowedBy (char '_' <|> alphaNum)

symbols :: [(Char, TokenType)]
symbols =
  [ ('+', Plus)
  , ('-', Minus)
  , (',', Comma)
  , (':', Colon)
  , (';', Semicolon)
  , ('[', LBracket)
  , (']', RBracket)
  , ('<', LAngle)
  , ('>', RAngle)
  ]

symbol :: (Char, TokenType) -> Parser (String, TokenType)
symbol (c, t) = do
  void $ char c
  pure ([c], t)