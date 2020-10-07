module Data.VCS.Ignore.RepoPath where


newtype RepoPath = RepoPath [String] deriving (Eq, Show)

instance Semigroup RepoPath where
    RepoPath p1 <> RepoPath p2 = RepoPath $ p1 <> p2
