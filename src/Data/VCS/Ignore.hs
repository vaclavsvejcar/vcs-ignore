{-|
Module      : Data.VCS.Ignore
Description : Reexported modules for convenience
Copyright   : (c) 2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Data.VCS.Ignore
  ( -- Working with ignored files
    findRepo
  , listRepo
  , walkRepo
    -- * Repo /type class/
  , Repo(..)
  , RepoError(..)
    -- * /GIT/ implementation
  , Git(..)
    -- * Common data types
  , VCSIgnoreError(..)
  )
where

import           Data.VCS.Ignore.Core           ( findRepo
                                                , listRepo
                                                , walkRepo
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           Data.VCS.Ignore.Repo.Git       ( Git(..) )
import           Data.VCS.Ignore.Types          ( VCSIgnoreError(..) )
