![CI](https://github.com/vaclavsvejcar/vcs-ignore/workflows/CI/badge.svg)

# vcs-ignore
`vcs-ignore` is small Haskell library used to find, check and process files ignored by selected _VCS_.

## 1. Table of Contents
<!-- TOC -->

- [1. Table of Contents](#1-table-of-contents)
- [2. Example of Use](#2-example-of-use)
    - [2.1. Listing all files/directories ignored by VCS](#21-listing-all-filesdirectories-ignored-by-vcs)
    - [2.2. Walking files/directories ignored by VCS](#22-walking-filesdirectories-ignored-by-vcs)
    - [2.3. Checking if path is ignored by VCS](#23-checking-if-path-is-ignored-by-vcs)

<!-- /TOC -->


## 2. Example of Use
Because this library is really simple to use, following example should be enough to understand how to use it for your project.

### 2.1. Listing all files/directories ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..), listRepo )

example :: IO [FilePath]
example = do
  repo <- scanRepo @Git "path/to/repo"
  listRepo repo
```

### 2.2. Walking files/directories ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.Maybe       ( catMaybes )
import System.Directory ( doesFileExist )
import Data.VCS.Ignore  ( Git, Repo(..), walkRepo )

onlyFiles :: IO [FilePath]
onlyFiles = do
  repo <- scanRepo @Git "path/to/repo"
  catMaybes <$> walkRepo repo walkFn
 where
  walkFn path = do
    file <- doesFileExist path
    pure (if file then Just path else Nothing)

```

### 2.3. Checking if path is ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..) )

checkIgnored :: IO Bool
checkIgnored = do
  repo <- scanRepo @Git "path/to/repo"
  isIgnored repo "/some/path/.DS_Store"
```
