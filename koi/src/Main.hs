{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStr, putStrLn)
import Control.Monad
import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding
import Data.Text.IO
import Koi.Board
--import Koi.Flag
import Koi.Parser
import Koi.Program
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Megaparsec.Error

main :: IO ()
main = do
  args <- getArgs
  let (_, inputs, errors) = getOpt Permute [] args
  if null errors then
    if length inputs == 1 then do
      let fileName = head inputs
      code <- decodeUtf8 <$> B.readFile fileName
      case parseProgram fileName code of
        Right program -> do
          result <- evalProgram program
          case result of
            Left err -> do
              putStrLn err
              exitWith (ExitFailure 3)
            Right board -> do
              boardString <- showBoard board
              putStr boardString
        Left err -> do
          putStr . pack $ errorBundlePretty err
          exitWith (ExitFailure 3)
    else do
      putStrLn "Expected a command line argument containing the name of the file to execute."
      exitWith (ExitFailure 2)
  else do
    forM_ errors (putStrLn . pack)
    exitWith (ExitFailure 1)