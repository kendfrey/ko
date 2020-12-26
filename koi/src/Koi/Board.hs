module Koi.Board
  ( Board
  , BoardPos(..)
  , newBoard
  , readBoard
  , showBoard
  ) where

import Data.Array.IO

data BoardPos = Empty | Black | White
  deriving Show

type Board = IOArray (Int, Int) BoardPos

newBoard :: (Int, Int) -> IO Board
newBoard (w, h) = newArray ((0, 0), (w - 1, h - 1)) Empty

readBoard :: Board -> (Int, Int) -> IO BoardPos
readBoard = readArray

showBoard :: Board -> IO String
showBoard board = do
  (_, ub) <- getBounds board
  showBoardImpl (0, 0) ub board

showBoardImpl :: (Int, Int) -> (Int, Int) -> Board -> IO String
showBoardImpl (x, y) ub@(ubx, uby) board =
  if y > uby then
    pure ""
  else if x > ubx then
    ('\n' :) <$> showBoardImpl (0, y + 1) ub board
  else do
    stone <- readArray board (x, y)
    (stoneChar stone :) . (' ' :) <$> showBoardImpl (x + 1, y) ub board

stoneChar :: BoardPos -> Char
stoneChar Empty = '.'
stoneChar Black = 'X'
stoneChar White = 'O'