module Main where

import           Main.Options                   ( optionsParser )
import           Options.Applicative            ( execParser )


main :: IO ()
main = do
  options <- execParser optionsParser
  print options
