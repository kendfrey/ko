{-# LANGUAGE OverloadedStrings #-}

module Koi.Board
  ( Board
  , BoardPos(..)
  , Player(..)
  , newBoard
  , readBoard
  , showBoard
  , playStone
  ) where

import Control.Monad
import Data.Array.IO
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)

data Player = Black | White
  deriving (Eq, Show)

data BoardPos = Empty | Stone Player
  deriving (Eq, Show)

type Board = IOArray (Int, Int) BoardPos

newBoard :: (Int, Int) -> IO Board
newBoard (w, h) = newArray ((0, 0), (w - 1, h - 1)) Empty

readBoard :: Board -> (Int, Int) -> IO BoardPos
readBoard = readArray

writeBoard :: Board -> (Int, Int) -> BoardPos -> IO ()
writeBoard = writeArray

safeReadBoard :: Board -> (Int, Int) -> IO (Maybe BoardPos)
safeReadBoard board pos = do
  bounds <- getBounds board
  if inRange bounds pos then
    Just <$> readBoard board pos
  else
    pure Nothing

showBoard :: Board -> IO Text
showBoard board = do
  (lb, ub) <- getBounds board
  pack <$> showBoardImpl board ub lb

showBoardImpl :: Board -> (Int, Int) -> (Int, Int) -> IO String
showBoardImpl board ub@(ubx, uby) (x, y) =
  if y > uby then
    pure ""
  else if x > ubx then
    ('\n' :) <$> showBoardImpl board ub (0, y + 1)
  else do
    stone <- readBoard board (x, y)
    (stoneChar stone :) . (' ' :) <$> showBoardImpl board ub (x + 1, y)

stoneChar :: BoardPos -> Char
stoneChar Empty = '.'
stoneChar (Stone Black) = 'X'
stoneChar (Stone White) = 'O'

playStone :: Board -> (Int, Int) -> Player -> IO (Maybe Text)
playStone board pos player = do
  stone <- readBoard board pos
  case stone of
    Stone player' | player' == player -> pure Nothing -- If the stone already exists, do nothing
                  | otherwise -> pure $ Just "This move is already occupied." -- If the other player's stone is there, error.
    Empty -> do
      let adj = adjacent pos
      opponentStones <- filterM (isOpponent board player) adj
      deadOpponentStones <- filterM (isCapturable board pos) opponentStones
      isSuicide <- not <$> searchLiberty board player (S.singleton pos) (S.fromList adj)
      if isSuicide && null deadOpponentStones then
        pure $ Just "This move is suicide."
      else do
        mapM_ (killGroupFrom board) deadOpponentStones
        writeBoard board pos $ Stone player
        pure Nothing

isOpponent :: Board -> Player -> (Int, Int) -> IO Bool
isOpponent board player pos = do
  stone <- safeReadBoard board pos
  case stone of
    Just (Stone player') | player' /= player -> pure True
    _ -> pure False

isCapturable :: Board -> (Int, Int) -> (Int, Int) -> IO Bool
isCapturable board capturingStone pos = do
  Stone player <- readBoard board pos
  not <$> searchLiberty board player (S.singleton capturingStone) (S.singleton pos)

-- checked: the set of all positions that are known not to lead to a liberty
-- unchecked: the set of all positions that may lead to a liberty
searchLiberty :: Board -> Player -> Set (Int, Int) -> Set (Int, Int) -> IO Bool
searchLiberty board player checked unchecked = do
  case S.minView unchecked of -- Pop the next position to search
    Nothing -> pure False -- If there is nothing left to search, there are no liberties
    Just (pos, unchecked') -> do -- If there is a position to search
      bounds <- getBounds board
      if S.member pos checked || not (inRange bounds pos) then -- If this has already been checked or is outside the board
        searchLiberty board player checked unchecked' -- It's not a liberty so keep searching
      else do
        stone <- readBoard board pos -- Check what's at this position
        case stone of
          -- If it's empty, this is a liberty
          Empty -> pure True
          -- If it's part of the same group, consider it checked and check all surrounding positions
          Stone player' | player' == player -> searchLiberty board player (S.insert pos checked) (S.union unchecked' . S.fromList $ adjacent pos)
          -- If it's the other player, it's not a liberty so consider it checked and keep searching
          _ -> searchLiberty board player (S.insert pos checked) unchecked'

killGroupFrom :: Board -> (Int, Int) -> IO ()
killGroupFrom board pos = do
  Stone player <- readBoard board pos
  killGroup board player pos

killGroup :: Board -> Player -> (Int, Int) -> IO ()
killGroup board player pos = do
  stone <- safeReadBoard board pos
  case stone of
    Just (Stone player') | player' == player -> do
      writeBoard board pos Empty
      mapM_ (killGroup board player) (adjacent pos)
    _ -> pure ()

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]