{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Text.Encoding
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
          result <- runExceptT $ evalProgram program
          case result of
            Right board -> putStr =<< runReaderT showBoard board
            Left err -> do
              putStr $ errorBundlePretty err
              exitWith (ExitFailure 2)
        Left err -> do
          putStr $ errorBundlePretty err
          exitWith (ExitFailure 2)
    else do
      putStrLn "Expected a command line argument containing the name of the file to execute."
      exitWith (ExitFailure 1)
  else do
    forM_ errors putStrLn
    exitWith (ExitFailure 1)