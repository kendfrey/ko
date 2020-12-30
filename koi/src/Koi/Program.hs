{-# LANGUAGE OverloadedStrings #-}

module Koi.Program
  ( Program(..)
  , Expr(..)
  , Pointer(..)
  , Command(..)
  , evalProgram
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Array.IArray
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Koi.Board

data Program = Program
  { programSize :: (Int, Int)
  , programLabels :: Map Text Int
  , programCode :: Array Int Command
  }
  deriving (Show)

data Expr
  = ELit Int
  | EPtr Pointer
  | EAdd Expr Expr
  | ESub Expr Expr
  deriving (Show)

data Pointer = Pointer
  { bits :: Expr
  , xOrigin :: Expr
  , yOrigin :: Expr
  , xStep :: Expr
  , yStep :: Expr
  }
  deriving (Show)

data Command
  = Goto Text
  | Pass
  | Play Player Pointer
  | If Expr Text
  | Case Expr (Array Int Text)
  | Copy Pointer Pointer
  deriving (Show)

data ProgramState = ProgramState { stateProgram :: Program, stateBoard :: Board, statePc :: Int, stateHalted :: Bool }

type ProgramResult = Board

type ExceptIO = ExceptT Text IO

evalProgram :: Program -> ExceptIO ProgramResult
evalProgram program = do
  board <- newBoard $ programSize program
  stateBoard <$> runProgram ProgramState { stateProgram = program, stateBoard = board, statePc = 0, stateHalted = False }

runProgram :: ProgramState -> ExceptIO ProgramState
runProgram state = do
  newState <- stepProgram state
  if stateHalted newState then
    pure newState
  else
    runProgram newState

stepProgram :: ProgramState -> ExceptIO ProgramState
stepProgram state = runCommand state $ (programCode . stateProgram $ state) ! statePc state -- TODO: error when end of program is reached

runCommand :: ProgramState -> Command -> ExceptIO ProgramState

runCommand state (Goto label) = jumpState state label

runCommand state Pass = pure state { stateHalted = True }

runCommand state (Play player (Pointer be xe ye dxe dye)) = do
  let board = stateBoard state
  b <- evalExpr board be
  x <- evalExpr board xe
  y <- evalExpr board ye
  dx <- evalExpr board dxe
  dy <- evalExpr board dye
  playPointer board player b x y dx dy
  stepState state

runCommand state (If expr label) = do
  value <- evalExpr (stateBoard state) expr
  if value /= 0 then
    jumpState state label
  else
    stepState state

runCommand state (Case expr labels) = do
  value <- evalExpr (stateBoard state) expr
  if inRange (bounds labels) value then
    jumpState state $ labels ! value
  else
    stepState state

runCommand state (Copy (Pointer fbe fxe fye fdxe fdye) (Pointer tbe txe tye tdxe tdye)) = do
  let board = stateBoard state
  fb <- evalExpr board fbe
  fx <- evalExpr board fxe
  fy <- evalExpr board fye
  fdx <- evalExpr board fdxe
  fdy <- evalExpr board fdye
  tb <- evalExpr board tbe
  tx <- evalExpr board txe
  ty <- evalExpr board tye
  tdx <- evalExpr board tdxe
  tdy <- evalExpr board tdye
  copyPointer board (min fb tb) fx fy fdx fdy tx ty tdx tdy
  stepState state

jumpState :: ProgramState -> Text -> ExceptIO ProgramState
jumpState state label = pure state { statePc = (programLabels $ stateProgram state) M.! label } -- TODO error msg

stepState :: ProgramState -> ExceptIO ProgramState
stepState state = pure state { statePc = statePc state + 1 }

playPointer :: Board -> Player -> Int -> Int -> Int -> Int -> Int -> ExceptIO ()
playPointer _ _ b _ _ _ _ | b <= 0 = pure ()
playPointer board player b x y dx dy = do
  playStone board (x, y) player
  playPointer board player (b - 1) (x + dx) (y + dy) dx dy

copyPointer :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ExceptIO ()
copyPointer _ b _ _ _ _ _ _ _ _ | b <= 0 = pure ()
copyPointer board b fx fy fdx fdy tx ty tdx tdy = do
  stone <- readBoard board (fx, fy)
  case stone of
    Stone player -> playStone board (tx, ty) player
    Empty -> pure ()
  copyPointer board (b - 1) (fx + fdx) (fy + fdy) fdx fdy (tx + tdx) (ty + tdy) tdx tdy

toBit :: BoardPos -> Int
toBit Empty = 0
toBit _ = 1

readPointer :: Board -> Int -> Int -> Int -> Int -> Int -> ExceptIO Int
readPointer _ b _ _ _ _ | b <= 0 = pure 0
readPointer board b x y dx dy = do
  bit <- readBoard board (x, y)
  rest <- readPointer board (b - 1) (x + dx) (y + dy) dx dy
  pure $ rest * 2 + toBit bit

evalExpr :: Board -> Expr -> ExceptIO Int
evalExpr _ (ELit x) = pure x
evalExpr board (EPtr (Pointer b x y dx dy)) = join $ readPointer board <$> evalExpr board b <*> evalExpr board x <*> evalExpr board y <*> evalExpr board dx <*> evalExpr board dy
evalExpr board (EAdd x y) = (+) <$> evalExpr board x <*> evalExpr board y
evalExpr board (ESub x y) = (-) <$> evalExpr board x <*> evalExpr board y