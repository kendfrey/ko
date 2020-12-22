module Koi.Program where

import Control.Monad.Reader
import Data.Array.IArray
import Data.Array.IO
import Data.Map (Map)
import qualified Data.Map as M

data Program = Program
  { size :: (Int, Int)
  , declarations :: Map String Expr
  , labels :: Map String Int
  , code :: Array Int Command
  }

data BoardPos = Empty | Black | White

type Board = IOArray (Int, Int) BoardPos

data Value
  = Lit Int
  | Decl String
  | Ptr Pointer

data Expr = EValue Value | EAdd Expr Value | ESub Expr Value

data Pointer = Pointer
  { bits :: Expr
  , xOrigin :: Expr
  , yOrigin :: Expr
  , xStep :: Expr
  , yStep :: Expr
  }

data Mode = StoneMode | BlackMode | WhiteMode

data Command
  = Goto String
  | Pass
  | PlayBlack Pointer
  | PlayWhite Pointer
  | If Mode Pointer String
  | Table Mode Pointer (Array Int String)
  | Copy Pointer Pointer

readBit :: BoardPos -> Mode -> Int
readBit Empty _ = 0
readBit _ StoneMode = 1
readBit Black BlackMode = 1
readBit White WhiteMode = 1
readBit _ _ = 0

readPointer :: Int -> Int -> Int -> Int -> Int -> ReaderT (Map String Expr, Board, Mode) IO Int
readPointer b _ _ _ _ | b <= 0 = pure 0
readPointer b x y dx dy = do
  (_, board, mode) <- ask
  bit <- liftIO (readArray board (x, y))
  let lsb = readBit bit mode
  rest <- readPointer (b - 1) (x + dx) (y + dy) dx dy
  pure (rest * 2 + lsb)

evalExpr :: Expr -> ReaderT (Map String Expr, Board, Mode) IO Int
evalExpr (EValue x) = evalValue x
evalExpr (EAdd e x) = (+) <$> evalExpr e <*> evalValue x
evalExpr (ESub e x) = (-) <$> evalExpr e <*> evalValue x

evalValue :: Value -> ReaderT (Map String Expr, Board, Mode) IO Int
evalValue (Lit x) = pure x
evalValue (Decl x) = do
  (decl, _, _) <- ask
  evalExpr (decl M.! x)
evalValue (Ptr (Pointer b x y dx dy)) = join $ readPointer <$> evalExpr b <*> evalExpr x <*> evalExpr y <*> evalExpr dx <*> evalExpr dy
