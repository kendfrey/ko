module Main where

import Control.Monad
--import Koi.Flag
import Koi.Lexer
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  let (_, inputs, errors) = getOpt Permute [] args
  if null errors then
    if length inputs == 1 then do
      let fileName = head inputs
      code <- readFile fileName
      case parse lexer fileName code of
        Right parsed ->
          forM_ (tokenValue <$> parsed) putStrLn
        Left err ->
          print err
    else do
      putStrLn "Expected a command line argument containing the name of the file to execute."
      exitWith (ExitFailure 2)
  else do
    forM_ errors putStrLn
    exitWith (ExitFailure 1)
