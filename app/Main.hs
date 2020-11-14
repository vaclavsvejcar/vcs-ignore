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
import           Data.VCS.Ignore.RepoPath       ( fromFilePath )
import           Main.Options                   ( Mode(..)
                                                , Options(..)
                                                , optionsParser
                                                )
import           Options.Applicative            ( execParser )
import           System.Directory               ( getCurrentDirectory )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )


main :: IO ()
main = do
  options <- execParser optionsParser
  repo    <- findRepoOrFail @Git options
  executeSelected repo options


findRepoOrFail :: Repo r => Options -> IO r
findRepoOrFail Options {..} = do
  repoDir   <- maybe getCurrentDirectory pure oRepoRoot
  maybeRepo <- findRepo repoDir
  case maybeRepo of
    Just repo -> do
      putStrLn $ "Found repository at: " <> repoRoot repo
      pure repo
    Nothing -> do
      putStrLn $ "No repository found for path: " <> repoDir
      exitFailure


executeSelected :: Repo r => r -> Options -> IO ()
executeSelected repo (oMode -> Path path) = checkPath repo path


checkPath :: Repo r => r -> FilePath -> IO ()
checkPath repo path | isExcluded repo (fromFilePath path) = reportIgnored
                    | otherwise                           = reportNotIgnored
 where
  reportIgnored = do
    putStrLn $ "Path '" <> path <> "' is ignored"
    exitSuccess
  reportNotIgnored = do
    putStrLn $ "Path '" <> path <> "' is not ignored"
    exitFailure
