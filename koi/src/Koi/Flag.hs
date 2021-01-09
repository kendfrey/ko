module Koi.Flag
  ( Options(..)
  , parseOptions
  , showHelp
  ) where

import System.Console.GetOpt

data Flag
  = SourceFile String
  | InFile String
  | OutFile String

data Options = Options { sourceFile :: Maybe String, inFile :: Maybe String, outFile :: Maybe String }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing

parseOptions :: [String] -> (Options, [String])
parseOptions args = (foldr foldFlag defaultOptions $ opts ++ map SourceFile inputs, errors)
  where
    (opts, inputs, errors) = getOpt Permute flags args

showHelp :: IO ()
showHelp = putStrLn $ usageInfo "Ko Interpreter" flags

flags :: [OptDescr Flag]
flags =
  [ Option ['i'] ["in"] (ReqArg InFile "FILE") "The input board file"
  , Option ['o'] ["out"] (ReqArg OutFile "FILE") "The output board file"
  ]

foldFlag :: Flag -> Options -> Options
foldFlag (SourceFile name) opts = opts { sourceFile = Just name }
foldFlag (InFile name) opts = opts { inFile = Just name }
foldFlag (OutFile name) opts = opts { outFile = Just name }