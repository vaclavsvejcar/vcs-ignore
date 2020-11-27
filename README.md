![CI](https://github.com/vaclavsvejcar/vcs-ignore/workflows/CI/badge.svg)

# vcs-ignore
`vcs-ignore` is small Haskell library used to find, check and process files ignored by selected _VCS_.

## Example of Use
Because this library is really simple to use, following example should be enough to understand how to use it for your project.

### Listing all files/directories ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..), listRepo )

example :: IO [FilePath]
example = do
  repo <- scanRepo @Git "path/to/repo"
  listRepo repo
```

### Walking files/directories ignored by VCS
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

### Checking if path is ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..) )

onlyFiles :: IO Bool
onlyFiles = do
  repo <- scanRepo @Git "path/to/repo"
  isIgnored repo "/some/path/.DS_Store"
```
