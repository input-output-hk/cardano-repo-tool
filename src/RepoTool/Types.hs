{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Types
  ( ConfigType (..)
  , GitHash (..)
  , RepoDirectory (..)
  , RepoInfo (..)
  , RepoInfoMap
  , RepoName (..)
  , RepoUrl (..)
  , TextPart (..)

  , gitNameFromUrl
  , repoMapify
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data ConfigType
  = ConfigCabalProject
  | ConfigStackYaml
  deriving (Eq, Show)

newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Show)

newtype RepoDirectory
  = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Show)

data RepoInfo = RepoInfo
  { riDirectory :: !RepoDirectory
  , riHash :: !GitHash
  , riName :: !RepoName
  , riUrl :: !RepoUrl
  }
  deriving (Eq, Show)

type RepoInfoMap
  = Map RepoName RepoInfo

newtype RepoName
  = RepoName { unRepoName :: Text }
  deriving (Eq, Ord, Show)

newtype RepoUrl
  = RepoUrl { unRepoUrl :: Text }
  deriving (Eq, Show)

data TextPart
  = TextWhitespace !Text
  | TextReadable !Text
  | TextGitHash !Text
  | TextGitRepo !Text
  deriving (Eq, Show)


gitNameFromUrl :: RepoUrl -> RepoName
gitNameFromUrl (RepoUrl txt) =
  RepoName (Text.takeWhile (/= '.') . List.last $ Text.splitOn "/" txt)

repoMapify :: [RepoInfo] -> RepoInfoMap
repoMapify =
  Map.fromList . map (\ ri -> (riName ri, ri))
