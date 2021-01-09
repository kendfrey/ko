{-# LANGUAGE TupleSections #-}

module Koi.Program
  ( Program(..)
  , Expr(..)
  , PtrExpr(..)
  , Pointer(..)
  , Command(..)
  , CommandInfo
  , evalProgram
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Data.Array.IArray
import Data.List.NonEmpty
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Koi.Board
import Text.Megaparsec

data Program = Program
  { programSize :: (Integer, Integer)
  , programCode :: Array Integer CommandInfo
  , programStart :: PosState Text
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

type CommandInfo = (Int, Int, Command)

data ProgramState = ProgramState { stateProgram :: Program, stateBoard :: Board, statePc :: Integer, stateHalted :: Bool }

type ProgramResult = Board

type Run = StateT ProgramState (ExceptT String IO)

evalProgram :: Program -> Board -> RunParsed ProgramResult
evalProgram program board = stateBoard <$> execStateT runProgram ProgramState { stateProgram = program, stateBoard = board, statePc = 0, stateHalted = False }

runProgram :: StateT ProgramState RunParsed ()
runProgram = do
  stepProgram
  halted <- gets stateHalted
  if halted then
    pure ()
  else
    runProgram

stepProgram :: StateT ProgramState RunParsed ()
stepProgram = do
  state <- get
  let (start, _, cmd) = programCode (stateProgram state) ! statePc state
  mapStateT (withExceptT . makeParseError start . programStart $ stateProgram state) $ runCommand cmd

makeParseError :: Int -> PosState Text -> String -> ParseErrorBundle Text Void
makeParseError start posState msg = ParseErrorBundle ((:| []) . FancyError start . S.singleton . ErrorFail $ "Runtime error: " ++ msg) posState

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
  if inRange (bounds . programCode $ stateProgram state) pos then
    put state { statePc = pos }
  else
    throwError $ "Cannot go to " ++ show pos ++ " because there is no code at that location."

stepState :: Run ()
stepState = do
  pos <- gets statePc
  jumpState (pos + 1)

playPointer :: Player -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard ()
playPointer _ _ b _ _ _ _ | b <= 0 = pure ()
playPointer player val b x y dx dy = do
  if odd val then
    playStone (y, x) player
  else
    pure ()
  playPointer player (val `div` 2) (b - 1) (x + dx) (y + dy) dx dy

copyPointer :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard ()
copyPointer b _ _ _ _ _ _ _ _ | b <= 0 = pure ()
copyPointer b tx ty tdx tdy fx fy fdx fdy = do
  stone <- readBoard (fy, fx)
  case stone of
    Stone player -> playStone (ty, tx) player
    Empty -> pure ()
  copyPointer (b - 1) (tx + tdx) (ty + tdy) tdx tdy (fx + fdx) (fy + fdy) fdx fdy

toBit :: BoardPos -> Integer
toBit Empty = 0
toBit _ = 1

readPointer :: Integer -> Integer -> Integer -> Integer -> Integer -> RunBoard Integer
readPointer b _ _ _ _ | b <= 0 = pure 0
readPointer b x y dx dy = do
  bit <- readBoard (y, x)
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