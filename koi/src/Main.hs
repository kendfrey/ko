module Main where

import Control.Monad
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
      code <- readFile fileName
      case parseProgram fileName code of
        Right prog -> do
          result <- evalProgram prog
          boardString <- showBoard result
          putStr boardString
        Left err -> do
          putStr $ errorBundlePretty err
          exitWith (ExitFailure 3)
    else do
      putStrLn "Expected a command line argument containing the name of the file to execute."
      exitWith (ExitFailure 2)
  else do
    forM_ errors putStrLn
    exitWith (ExitFailure 1)