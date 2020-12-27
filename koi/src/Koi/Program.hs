module Koi.Program
  ( Program(..)
  , Expr(..)
  , Pointer(..)
  , Command(..)
  , evalProgram
  ) where

import Control.Monad
import Data.Array.IArray
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Koi.Board

data Program = Program
  { programSize :: (Int, Int)
  , programLabels :: Map String Int
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
  = Goto String
  | Pass
  | Play Player Pointer
  | If Expr String
  | Case Expr (Array Int String)
  | Copy Pointer Pointer
  deriving (Show)

data ProgramState = ProgramState { stateProgram :: Program, stateBoard :: Board, statePc :: Int, stateHalted :: Bool }

type ProgramResult = Board

evalProgram :: Program -> IO (Either String ProgramResult)
evalProgram program = do
  board <- newBoard $ programSize program
  endState <- runProgram ProgramState { stateProgram = program, stateBoard = board, statePc = 0, stateHalted = False }
  pure $ stateBoard <$> endState

runProgram :: ProgramState -> IO (Either String ProgramState)
runProgram state = do
  newState <- stepProgram state
  if either (const True) stateHalted newState then
    pure newState
  else
    runProgram $ fromRight undefined newState

stepProgram :: ProgramState -> IO (Either String ProgramState)
stepProgram state = runCommand state $ (programCode . stateProgram $ state) ! statePc state

runCommand :: ProgramState -> Command -> IO (Either String ProgramState)

runCommand state (Goto label) = pure . Right $ jumpState state label

runCommand state Pass = pure . Right $ state { stateHalted = True }

runCommand state (Play player (Pointer be xe ye dxe dye)) = do
  let board = stateBoard state
  b <- evalExpr board be
  x <- evalExpr board xe
  y <- evalExpr board ye
  dx <- evalExpr board dxe
  dy <- evalExpr board dye
  result <- playPointer board player b x y dx dy
  case result of
    Nothing -> pure . Right $ stepState state
    Just err -> pure $ Left err

runCommand state (If expr label) = do
  value <- evalExpr (stateBoard state) expr
  if value /= 0 then
    pure . Right $ jumpState state label
  else
    pure . Right $ stepState state

runCommand state (Case expr labels) = do
  value <- evalExpr (stateBoard state) expr
  if inRange (bounds labels) value then do
    pure . Right . jumpState state $ labels ! value
  else
    pure . Right $ stepState state

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
  result <- copyPointer board (min fb tb) fx fy fdx fdy tx ty tdx tdy
  case result of
    Nothing -> pure . Right $ stepState state
    Just err -> pure $ Left err

jumpState :: ProgramState -> String  -> ProgramState
jumpState state label = state { statePc = (programLabels $ stateProgram state) M.! label }

stepState :: ProgramState -> ProgramState
stepState state = state { statePc = statePc state + 1 }

playPointer :: Board -> Player -> Int -> Int -> Int -> Int -> Int -> IO (Maybe String)
playPointer _ _ b _ _ _ _ | b <= 0 = pure Nothing
playPointer board player b x y dx dy = do
  result <- playStone board (x, y) player
  case result of
    Nothing -> playPointer board player (b - 1) (x + dx) (y + dy) dx dy
    Just err -> pure $ Just err

copyPointer :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Maybe String)
copyPointer _ b _ _ _ _ _ _ _ _ | b <= 0 = pure Nothing
copyPointer board b fx fy fdx fdy tx ty tdx tdy = do
  stone <- readBoard board (fx, fy)
  result <-
    case stone of
      Stone player -> playStone board (tx, ty) player
      Empty -> pure Nothing
  case result of
    Nothing -> copyPointer board (b - 1) (fx + fdx) (fy + fdy) fdx fdy (tx + tdx) (ty + tdy) tdx tdy
    Just err -> pure $ Just err

toBit :: BoardPos -> Int
toBit Empty = 0
toBit _ = 1

readPointer :: Board -> Int -> Int -> Int -> Int -> Int -> IO Int
readPointer _ b _ _ _ _ | b <= 0 = pure 0
readPointer board b x y dx dy = do
  bit <- readBoard board (x, y)
  rest <- readPointer board (b - 1) (x + dx) (y + dy) dx dy
  pure (rest * 2 + toBit bit)

evalExpr :: Board -> Expr -> IO Int
evalExpr _ (ELit x) = pure x
evalExpr board (EPtr (Pointer b x y dx dy)) = join $ readPointer board <$> evalExpr board b <*> evalExpr board x <*> evalExpr board y <*> evalExpr board dx <*> evalExpr board dy
evalExpr board (EAdd x y) = (+) <$> evalExpr board x <*> evalExpr board y
evalExpr board (ESub x y) = (-) <$> evalExpr board x <*> evalExpr board y