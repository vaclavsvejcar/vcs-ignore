{-# LANGUAGE StrictData #-}

{-|
Module      : Main.Options
Description : Options definitions for "optparse-applicative"
Copyright   : (c) 2020-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Options definitions for "optparse-applicative".
-}

module Main.Options
  ( Options(..)
  , Mode(..)
  , optionsParser
  )
where

import           Main.Vendor                    ( productDesc
                                                , productInfo
                                                )
import           Options.Applicative


data Options = Options
  { oMode  :: Mode
  , oDebug :: Bool
  }
  deriving (Eq, Show)

data Mode = Path FilePath
  deriving (Eq, Show)


optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper)
                     (fullDesc <> progDesc productDesc <> header productInfo)
 where
  options =
    Options
      <$> (   Path
          <$> strOption
                (long "path" <> short 'p' <> metavar "PATH" <> help
                  "path to check"
                )
          )
      <*> switch (long "debug" <> help "produce more verbose output")
