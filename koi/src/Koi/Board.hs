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
import Control.Monad.Except
import Data.Array.IO
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)

data Player = Black | White
  deriving (Eq, Show)

data BoardPos = Empty | Stone Player
  deriving (Eq, Show)

type Board = IOArray (Int, Int) BoardPos

type ExceptIO = ExceptT Text IO

newBoard :: (Int, Int) -> ExceptIO Board
newBoard (w, h) = liftIO $ newArray ((0, 0), (w - 1, h - 1)) Empty

safeReadBoard :: Board -> (Int, Int) -> ExceptIO (Maybe BoardPos)
safeReadBoard board pos = do
  bounds <- liftIO $ getBounds board
  if inRange bounds pos then
    liftIO $ Just <$> readArray board pos
  else
    pure Nothing

readBoard :: Board -> (Int, Int) -> ExceptIO BoardPos
readBoard board pos = do
  stone <- safeReadBoard board pos
  case stone of
    Just s -> pure s
    Nothing -> throwError "This move is outside the board."

writeBoard :: Board -> (Int, Int) -> BoardPos -> ExceptIO ()
writeBoard board pos stone = do
  bounds <- liftIO $ getBounds board
  if inRange bounds pos then
    liftIO $ writeArray board pos stone
  else
    throwError "This move is outside the board."

showBoard :: Board -> ExceptIO Text
showBoard board = do
  (_, ub) <- liftIO $ getBounds board
  pack <$> showBoardImpl board ub (0, 0)

showBoardImpl :: Board -> (Int, Int) -> (Int, Int) -> ExceptIO String
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

playStone :: Board -> (Int, Int) -> Player -> ExceptIO ()
playStone board pos player = do
  stone <- readBoard board pos
  case stone of
    Stone player' | player' == player -> pure () -- If the stone already exists, do nothing
                  | otherwise -> throwError "This move is already occupied." -- If the other player's stone is there, error.
    Empty -> do
      let adj = adjacent pos
      opponentStones <- filterM (isOpponent board player) adj
      deadOpponentStones <- filterM (isCapturable board pos) opponentStones
      isSuicide <- not <$> searchLiberty board player (S.singleton pos) (S.fromList adj)
      if isSuicide && null deadOpponentStones then
        throwError "This move is suicide."
      else do
        mapM_ (killGroupFrom board) deadOpponentStones
        writeBoard board pos $ Stone player

isOpponent :: Board -> Player -> (Int, Int) -> ExceptIO Bool
isOpponent board player pos = do
  stone <- safeReadBoard board pos
  case stone of
    Just (Stone player') | player' /= player -> pure True
    _ -> pure False

isCapturable :: Board -> (Int, Int) -> (Int, Int) -> ExceptIO Bool
isCapturable board capturingStone pos = do
  Stone player <- readBoard board pos
  not <$> searchLiberty board player (S.singleton capturingStone) (S.singleton pos)

-- checked: the set of all positions that are known not to lead to a liberty
-- unchecked: the set of all positions that may lead to a liberty
searchLiberty :: Board -> Player -> Set (Int, Int) -> Set (Int, Int) -> ExceptIO Bool
searchLiberty board player checked unchecked =
  case S.minView unchecked of -- Pop the next position to search
    Nothing -> pure False -- If there is nothing left to search, there are no liberties
    Just (pos, unchecked') -> do -- If there is a position to search
      bounds <- liftIO $ getBounds board
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

killGroupFrom :: Board -> (Int, Int) -> ExceptIO ()
killGroupFrom board pos = do
  Stone player <- readBoard board pos
  killGroup board player pos

killGroup :: Board -> Player -> (Int, Int) -> ExceptIO ()
killGroup board player pos = do
  stone <- safeReadBoard board pos
  case stone of
    Just (Stone player') | player' == player -> do
      writeBoard board pos Empty
      mapM_ (killGroup board player) (adjacent pos)
    _ -> pure ()

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]