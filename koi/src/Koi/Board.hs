module Koi.Board
  ( Board
  , BoardPos(..)
  , Player(..)
  , RunBoard
  , RunParsed
  , newBoard
  , readBoard
  , showBoard
  , playStone
  , parseBoard
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IO
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Board = IOArray (Integer, Integer) BoardPos

data BoardPos = Empty | Stone Player

data Player = Black | White
  deriving (Eq)

type RunBoard = ReaderT Board (ExceptT String IO)

type RunParsed = ExceptT (ParseErrorBundle Text Void) IO

newBoard :: (Integer, Integer) -> IO Board
newBoard (h, w) = newArray ((0, 0), (h - 1, w - 1)) Empty

safeReadBoard :: (Integer, Integer) -> RunBoard (Maybe BoardPos)
safeReadBoard pos = do
  board <- ask
  bounds <- liftIO $ getBounds board
  if inRange bounds pos then
    liftIO $ Just <$> readArray board pos
  else
    pure Nothing

readBoard :: (Integer, Integer) -> RunBoard BoardPos
readBoard pos = do
  stone <- safeReadBoard pos
  case stone of
    Just s -> pure s
    Nothing -> throwError $ "This move " ++ showMove pos ++ " is outside the board."

writeBoard :: (Integer, Integer) -> BoardPos -> RunBoard ()
writeBoard pos stone = do
  board <- ask
  bounds <- liftIO $ getBounds board
  if inRange bounds pos then
    liftIO $ writeArray board pos stone
  else
    throwError $ "This move " ++ showMove pos ++ " is outside the board."

showBoard :: ReaderT Board IO Text
showBoard = do
  (_, ub) <- liftIO =<< asks getBounds
  pack <$> showBoardImpl ub (0, 0)

showBoardImpl :: (Integer, Integer) -> (Integer, Integer) -> ReaderT Board IO String
showBoardImpl ub@(uby, ubx) (y, x)
  | y > uby = pure ""
  | x > ubx = ('\n' :) <$> showBoardImpl ub (y + 1, 0)
  | otherwise = do
    stone <- mapReaderT ((fromRight <$>) . runExceptT) $ readBoard (y, x)
    (stoneChar stone :) . (' ' :) <$> showBoardImpl ub (y, x + 1)
  where
    fromRight (Right r) = r
    fromRight _ = error "fromRight"

stoneChar :: BoardPos -> Char
stoneChar Empty = '.'
stoneChar (Stone Black) = 'X'
stoneChar (Stone White) = 'O'

playStone :: (Integer, Integer) -> Player -> RunBoard ()
playStone pos player = do
  stone <- readBoard pos
  case stone of
    Stone player'
      | player' == player -> pure () -- If the stone already exists, do nothing
      | otherwise -> throwError $ "This move " ++ showMove pos ++ " is already occupied." -- If the other player's stone is there, error.
    Empty -> do
      let adj = adjacent pos
      opponentStones <- filterM (isOpponent player) adj
      deadOpponentStones <- filterM (isCapturable pos) opponentStones
      isSuicide <- not <$> searchLiberty player (S.singleton pos) (S.fromList adj)
      if isSuicide && null deadOpponentStones then
        throwError $ "This move " ++ showMove pos ++ " is suicide."
      else do
        mapM_ killGroupFrom deadOpponentStones
        writeBoard pos $ Stone player

isOpponent :: Player -> (Integer, Integer) -> RunBoard Bool
isOpponent player pos = do
  stone <- safeReadBoard pos
  case stone of
    Just (Stone player') | player' /= player -> pure True
    _ -> pure False

isCapturable :: (Integer, Integer) -> (Integer, Integer) -> RunBoard Bool
isCapturable capturingStone pos = do
  Stone player <- readBoard pos
  not <$> searchLiberty player (S.singleton capturingStone) (S.singleton pos)

-- checked: the set of all positions that are known not to lead to a liberty
-- unchecked: the set of all positions that may lead to a liberty
searchLiberty :: Player -> Set (Integer, Integer) -> Set (Integer, Integer) -> RunBoard Bool
searchLiberty player checked unchecked =
  case S.minView unchecked of -- Pop the next position to search
    Nothing -> pure False -- If there is nothing left to search, there are no liberties
    Just (pos, unchecked') -> do -- If there is a position to search
      bounds <- liftIO =<< asks getBounds
      if S.member pos checked || not (inRange bounds pos) then -- If this has already been checked or is outside the board
        searchLiberty player checked unchecked' -- It's not a liberty so keep searching
      else do
        stone <- readBoard pos -- Check what's at this position
        case stone of
          -- If it's empty, this is a liberty
          Empty -> pure True
          -- If it's part of the same group, consider it checked and check all surrounding positions
          Stone player' | player' == player -> searchLiberty player (S.insert pos checked) (S.union unchecked' . S.fromList $ adjacent pos)
          -- If it's the other player, it's not a liberty so consider it checked and keep searching
          _ -> searchLiberty player (S.insert pos checked) unchecked'

killGroupFrom :: (Integer, Integer) -> RunBoard ()
killGroupFrom pos = do
  Stone player <- readBoard pos
  killGroup player pos

killGroup :: Player -> (Integer, Integer) -> RunBoard ()
killGroup player pos = do
  stone <- safeReadBoard pos
  case stone of
    Just (Stone player') | player' == player -> do
      writeBoard pos Empty
      mapM_ (killGroup player) (adjacent pos)
    _ -> pure ()

adjacent :: (Integer, Integer) -> [(Integer, Integer)]
adjacent (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

showMove :: (Integer, Integer) -> String
showMove (y, x) = "[" ++ show x ++ "," ++ show y ++ "]"

type Parser = Parsec Void Text

parseBoard :: (Integer, Integer) -> String -> Text -> RunParsed Board
parseBoard size fileName input = do
  case runParser (boardParser size) fileName input of
    Right board -> liftIO board
    Left err -> throwError err

boardParser :: (Integer, Integer) -> Parser (IO Board)
boardParser (height, width) = newListArray ((0, 0), (height - 1, width - 1)) . join <$> count (fromInteger height) (lineParser width) <* eof

lineParser :: Integer -> Parser [BoardPos]
lineParser width = hspace *> count (fromInteger width) cellParser <* eol

cellParser :: Parser BoardPos
cellParser = (char '.' $> Empty <|> char 'X' $> Stone Black <|> char 'O' $> Stone White) <* hspace