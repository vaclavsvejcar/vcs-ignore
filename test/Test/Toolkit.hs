module Test.Toolkit
  ( matchesException
  )
where

import           Control.Exception              ( SomeException )
import           Control.Monad.Catch            ( Exception(..) )


matchesException :: Exception e
                 => (Maybe e -> Bool)
                 -> Either SomeException r
                 -> Bool
matchesException cond (Left ex) | cond (fromException ex) = True
                                | otherwise               = False
matchesException _ _ = False
