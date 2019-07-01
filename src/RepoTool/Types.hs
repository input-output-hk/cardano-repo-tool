{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Types
  ( GitHash (..)
  , RepoDirectory (..)
  , TextPart (..)
  ) where

import           Data.Text (Text)

newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Show)

newtype RepoDirectory
  = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Show)

data TextPart
  = TextWhitespace !Text
  | TextReadable !Text
  | TextGitHash !Text
  deriving (Eq, Show)
