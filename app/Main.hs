{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}


module Main
  ( main
  )
where

import           Data.VCS.Ignore.Core           ( findRepo )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.Repo.Git       ( Git )
import           Main.Options                   ( Mode(..)
                                                , Options(..)
                                                , optionsParser
                                                )
import           Options.Applicative            ( execParser )
import           System.Directory               ( canonicalizePath
                                                , getCurrentDirectory
                                                )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.FilePath                ( makeRelative )


main :: IO ()
main = do
  options <- execParser optionsParser
  repo    <- findRepoOrFail @Git options
  executeMode repo options


findRepoOrFail :: Repo r => Options -> IO r
findRepoOrFail Options {..} = do
  repoDir   <- getCurrentDirectory
  maybeRepo <- findRepo repoDir
  case maybeRepo of
    Just repo -> do
      putStrLn $ "Found repository at: " <> repoRoot repo
      pure repo
    Nothing -> do
      putStrLn $ "No repository found for path: " <> repoDir
      exitFailure


executeMode :: Repo r => r -> Options -> IO ()
executeMode repo (oMode -> Path path) = checkPath repo path


checkPath :: Repo r => r -> FilePath -> IO ()
checkPath repo path = do
  relative <- makeRelative (repoRoot repo) <$> canonicalizePath path
  excluded <- isExcluded repo relative
  if excluded then reportIgnored else reportNotIgnored
 where
  reportIgnored = do
    putStrLn $ "Path '" <> path <> "' IS ignored"
    exitSuccess
  reportNotIgnored = do
    putStrLn $ "Path '" <> path <> "' IS NOT ignored"
    exitFailure

