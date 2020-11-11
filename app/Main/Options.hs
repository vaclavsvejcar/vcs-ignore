{-# LANGUAGE StrictData #-}

module Main.Options
  ( Options(..)
  , Mode(..)
  , optionsParser
  )
where

import           Options.Applicative


data Options = Options
  { oMode :: Mode
  }
  deriving (Eq, Show)

data Mode = Path FilePath
  deriving (Eq, Show)


optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper)
                     (fullDesc <> progDesc "progDesc" <> header "header")
 where
  options = Options . Path <$> strOption
    (long "path" <> short 'p' <> metavar "PATH" <> help "path to check")
