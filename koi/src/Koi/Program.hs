module Koi.Program where

import Control.Monad.Reader
import Data.Array.IArray
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
  | PlayBlack Pointer
  | PlayWhite Pointer
  | If Expr String
  | Case Expr (Array Int String)
  | Copy Pointer Pointer
  deriving (Show)

data ProgramState = ProgramState { stateProgram :: Program, stateBoard :: Board, statePc :: Int, stateHalted :: Bool }

type ProgramResult = Board

evalProgram :: Program -> IO ProgramResult
evalProgram program = do
  board <- newBoard $ programSize program
  endState <- runProgram ProgramState { stateProgram = program, stateBoard = board, statePc = 0, stateHalted = False }
  pure $ stateBoard endState

runProgram :: ProgramState -> IO ProgramState
runProgram state = do
  newState <- stepProgram state
  if stateHalted newState then
    pure newState
  else
    runProgram newState

stepProgram :: ProgramState -> IO ProgramState
stepProgram state = runCommand state $ (programCode . stateProgram $ state) ! statePc state

runCommand :: ProgramState -> Command -> IO ProgramState
runCommand state (Goto l) = pure $ state { statePc = (programLabels . stateProgram $ state) M.! l }
runCommand state Pass = pure $ state { stateHalted = True }
runCommand _ _ = undefined

toBit :: BoardPos -> Int
toBit Empty = 0
toBit _ = 1

readPointer :: Int -> Int -> Int -> Int -> Int -> ReaderT Board IO Int
readPointer b _ _ _ _ | b <= 0 = pure 0
readPointer b x y dx dy = do
  board <- ask
  bit <- liftIO (readBoard board (x, y))
  rest <- readPointer (b - 1) (x + dx) (y + dy) dx dy
  pure (rest * 2 + toBit bit)

evalExpr :: Expr -> ReaderT Board IO Int
evalExpr (ELit x) = pure x
evalExpr (EPtr (Pointer b x y dx dy)) = join $ readPointer <$> evalExpr b <*> evalExpr x <*> evalExpr y <*> evalExpr dx <*> evalExpr dy
evalExpr (EAdd x y) = (+) <$> evalExpr x <*> evalExpr y
evalExpr (ESub x y) = (-) <$> evalExpr x <*> evalExpr y

toPointer :: Expr -> Maybe Pointer
toPointer (EPtr p) = Just p
toPointer (EAdd a b) = addPointer <$> toPointer a <*> toPointer b
toPointer (ESub a b) = subPointer <$> toPointer a <*> toPointer b
toPointer _ = Nothing

addPointer :: Pointer -> Pointer -> Pointer
addPointer (Pointer ab ax ay adx ady) (Pointer _ bx by bdx bdy) = Pointer ab (EAdd ax bx) (EAdd ay by) (EAdd adx bdx) (EAdd ady bdy)

subPointer :: Pointer -> Pointer -> Pointer
subPointer (Pointer ab ax ay adx ady) (Pointer _ bx by bdx bdy) = Pointer ab (ESub ax bx) (ESub ay by) (ESub adx bdx) (ESub ady bdy)