module Main (main) where

import Application.InputOutputFile (InputOutputFile (InputOutputFile), state, transactions)
import Application.PlayTransactions (playTransactions)
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decodeEither', encode)
import Lens.Micro
import Options.Applicative
import System.Exit (die)

data Input
  = FileInput FilePath
  | StdInput

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file path"
      )

stdInput :: Parser Input
stdInput =
  flag'
    StdInput
    ( long "stdin"
        <> help "Read input file from standard input (default)"
    )

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (fileInput <|> stdInput <|> pure StdInput <**> helper)
        ( fullDesc
            <> progDesc "Apply transactions on envelopes"
            <> header "envelopes - a simple budgeting tool"
        )

run :: Input -> IO ()
run (FileInput a) = (BS.readFile a) >>= play
run StdInput = BS.getContents >>= play

play :: BS.ByteString -> IO ()
play rawFile = do
  case (decodeEither' rawFile) of
    Left err -> die $ show err
    Right file -> do
      let state' = (file ^. state)
      let transactions' = (file ^. transactions)
      case playTransactions state' transactions' of
        Left err -> die $ show err
        Right newState -> BS.putStr $ encode $ InputOutputFile newState transactions'
