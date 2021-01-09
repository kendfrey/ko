module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Koi.Board
import Koi.Flag
import Koi.Parser
import Koi.Program
import System.Environment
import System.Exit
import Text.Megaparsec.Error

main :: IO ()
main = do
  args <- getArgs
  let (options, errors) = parseOptions args
  -- TODO: --help flag
  if null errors then
    case sourceFile options of
      Just sourceFile' -> do
        code <- decodeUtf8 <$> B.readFile sourceFile'
        result <- runExceptT $ do
          program <- ExceptT . pure $ parseProgram sourceFile' code
          inBoard <- getBoard (programSize program) (inFile options)
          evalProgram program inBoard
        case result of
          Right board -> do
            boardString <- runReaderT showBoard board
            putBoard (outFile options) boardString
          Left err -> do
            putStr $ errorBundlePretty err
            exitWith (ExitFailure 2)
      Nothing -> do
        putStrLn "Expected a command line argument containing the name of the file to execute."
        exitWith (ExitFailure 1)
  else do
    forM_ errors putStrLn
    exitWith (ExitFailure 1)

getBoard :: (Integer, Integer) -> Maybe String -> RunParsed Board
getBoard size Nothing = liftIO $ newBoard size
getBoard size (Just file) = do
  board <- liftIO $ decodeUtf8 <$> B.readFile file
  parseBoard size file board

putBoard :: Maybe String -> Text -> IO ()
putBoard Nothing = T.putStr
putBoard (Just file) = B.writeFile file . encodeUtf8