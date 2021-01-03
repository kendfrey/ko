{-# LANGUAGE OverloadedStrings #-}

module Koi.Board
  ( Board
  , BoardPos(..)
  , Player(..)
  , RunBoard
  , newBoard
  , readBoard
  , showBoard
  , playStone
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IO
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)

type Board = IOArray (Integer, Integer) BoardPos

data BoardPos = Empty | Stone Player

data Player = Black | White
  deriving (Eq)

type RunBoard = ReaderT Board (ExceptT Text IO)

newBoard :: (Integer, Integer) -> IO Board
newBoard (w, h) = newArray ((0, 0), (w - 1, h - 1)) Empty

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
    Nothing -> throwError "This move is outside the board."

writeBoard :: (Integer, Integer) -> BoardPos -> RunBoard ()
writeBoard pos stone = do
  board <- ask
  bounds <- liftIO $ getBounds board
  if inRange bounds pos then
    liftIO $ writeArray board pos stone
  else
    throwError "This move is outside the board."

showBoard :: RunBoard Text
showBoard = do
  (_, ub) <- liftIO =<< asks getBounds
  pack <$> showBoardImpl ub (0, 0)

showBoardImpl :: (Integer, Integer) -> (Integer, Integer) -> RunBoard String
showBoardImpl ub@(ubx, uby) (x, y)
  | y > uby = pure ""
  | x > ubx = ('\n' :) <$> showBoardImpl ub (0, y + 1)
  | otherwise = do
    stone <- readBoard (x, y)
    (stoneChar stone :) . (' ' :) <$> showBoardImpl ub (x + 1, y)

stoneChar :: BoardPos -> Char
stoneChar Empty = '.'
stoneChar (Stone Black) = 'X'
stoneChar (Stone White) = 'O'

playStone :: (Integer, Integer) -> Player -> RunBoard ()
playStone pos player = do
  stone <- readBoard pos
  case stone of
    Stone player' | player' == player -> pure () -- If the stone already exists, do nothing
                  | otherwise -> throwError "This move is already occupied." -- If the other player's stone is there, error.
    Empty -> do
      let adj = adjacent pos
      opponentStones <- filterM (isOpponent player) adj
      deadOpponentStones <- filterM (isCapturable pos) opponentStones
      isSuicide <- not <$> searchLiberty player (S.singleton pos) (S.fromList adj)
      if isSuicide && null deadOpponentStones then
        throwError "This move is suicide."
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
adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]