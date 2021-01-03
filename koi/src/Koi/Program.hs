{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Koi.Program
  ( Program(..)
  , Expr(..)
  , PtrExpr(..)
  , Pointer(..)
  , Command(..)
  , evalProgram
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Data.Array.IArray
import Data.Text (Text)
import Koi.Board

data Program = Program
  { programSize :: (Integer, Integer)
  , programCode :: Array Integer Command
  }

data Expr
  = Lit Integer
  | Ptr PtrExpr

data PtrExpr
  = PtrLit Pointer
  | Add PtrExpr PtrExpr
  | Sub PtrExpr PtrExpr

data Pointer = Pointer Expr Expr Expr Expr Expr

data Command
  = Goto Expr
  | Pass
  | Play Player PtrExpr Expr
  | If PtrExpr Expr
  | Copy PtrExpr PtrExpr

data ProgramState = ProgramState { stateProgram :: Program, stateBoard :: Board, statePc :: Integer, stateHalted :: Bool }

type ProgramResult = Board

type Run = StateT ProgramState (ExceptT Text IO)

evalProgram :: Program -> ExceptT Text IO ProgramResult
evalProgram program = do
  board <- liftIO . newBoard $ programSize program
  stateBoard <$> execStateT runProgram ProgramState { stateProgram = program, stateBoard = board, statePc = 0, stateHalted = False }

runProgram :: Run ()
runProgram = do
  stepProgram
  halted <- gets stateHalted
  if halted then
    pure ()
  else
    runProgram

stepProgram :: Run ()
stepProgram = do
  state <- get
  case programCode (stateProgram state) !? statePc state of
    Just cmd -> runCommand cmd -- TODO: include command information in error messages
    Nothing -> throwError "Execution moved outside the code." -- TODO: this can be checked in jumpState and will improve the error message

runCommand :: Command -> Run ()

runCommand (Goto pos) = jumpState =<< evalExpr' pos

runCommand Pass = do
  state <- get
  put state { stateHalted = True }

runCommand (Play player ptr expr) = do
  (b, x, y, dx, dy) <- evalPtr' ptr
  val <- evalExpr' expr
  withBoard $ playPointer player val b x y dx dy
  stepState

runCommand (If ptr pos) = do
  value <- evalExpr' $ Ptr ptr
  if value /= 0 then
    jumpState =<< evalExpr' pos
  else
    stepState

runCommand (Copy to from) = do
  (tb, tx, ty, tdx, tdy) <- evalPtr' to
  (fb, fx, fy, fdx, fdy) <- evalPtr' from
  withBoard $ copyPointer (min tb fb) tx ty tdx tdy fx fy fdx fdy
  stepState

jumpState :: Integer -> Run ()
jumpState pos = do
  state <- get
  put state { statePc = pos }

stepState :: Run ()
stepState = do
  state <- get
  put state { statePc = statePc state + 1 }

playPointer :: Player -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard ()
playPointer _ _ b _ _ _ _ | b <= 0 = pure ()
playPointer player val b x y dx dy = do
  if odd val then
    playStone (x, y) player
  else
    pure ()
  playPointer player (val `div` 2) (b - 1) (x + dx) (y + dy) dx dy

copyPointer :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard ()
copyPointer b _ _ _ _ _ _ _ _ | b <= 0 = pure ()
copyPointer b tx ty tdx tdy fx fy fdx fdy = do
  stone <- readBoard (fx, fy)
  case stone of
    Stone player -> playStone (tx, ty) player
    Empty -> pure ()
  copyPointer (b - 1) (tx + tdx) (ty + tdy) tdx tdy (fx + fdx) (fy + fdy) fdx fdy

toBit :: BoardPos -> Integer
toBit Empty = 0
toBit _ = 1

readPointer :: Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard Integer
readPointer b _ _ _ _ | b <= 0 = pure 0
readPointer b x y dx dy = do
  bit <- readBoard (x, y)
  rest <- readPointer (b - 1) (x + dx) (y + dy) dx dy
  pure $ rest * 2 + toBit bit

evalExpr' :: Expr -> Run Integer
evalExpr' = withBoard . evalExpr

evalExpr :: Expr -> RunBoard Integer
evalExpr (Lit x) = pure x
evalExpr (Ptr ptr) = do
  (b, x, y, dx, dy) <- evalPtr ptr
  readPointer b x y dx dy

evalPtr' :: PtrExpr -> Run (Integer, Integer, Integer, Integer, Integer)
evalPtr' = withBoard . evalPtr

evalPtr :: PtrExpr -> RunBoard (Integer, Integer, Integer, Integer, Integer)
evalPtr (PtrLit (Pointer b x y dx dy)) = (,,,,) <$> evalExpr b <*> evalExpr x <*> evalExpr y <*> evalExpr dx <*> evalExpr dy
evalPtr (Add a b) = pointerOp (+) <$> evalPtr a <*> evalPtr b
evalPtr (Sub a b) = pointerOp (-) <$> evalPtr a <*> evalPtr b

pointerOp :: (Integer -> Integer -> Integer) -> (Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer, Integer)
pointerOp op (ab, ax, ay, adx, ady) (_, bx, by, bdx, bdy) = (ab, op ax bx, op ay by, op adx bdx, op ady bdy)

withBoard :: RunBoard a -> Run a
withBoard r = lift . runReaderT r . stateBoard =<< get

(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
a !? i
  | inRange (bounds a) i = Just $ a ! i
  | otherwise = Nothing