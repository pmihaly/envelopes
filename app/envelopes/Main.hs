module Main (main) where

import Options.Applicative

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
run (FileInput a) = (readFile a) >>= putStrLn
run StdInput = getContents >>= putStrLn
