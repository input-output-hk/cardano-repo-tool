{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module RepoTool.Types
  ( ConfigType (..)
  , GitHash (..)
  , NixSha (..)
  , RepoDirectory (..)
  , RepoInfo (..)
  , RepoInfoMap
  , RepoName (..)
  , RepoUrl (..)
  , TextPart (..)

  , gitNameFromUrl
  , repoMapify
  , repoMapFilter
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data ConfigType
  = ConfigCabalProject
  | ConfigStackYaml
  deriving (Eq, Show)

newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Show)

newtype NixSha
  = NixSha { unNixSha :: Text }
  deriving (Eq, Show)

newtype RepoDirectory
  = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Show)

data RepoInfo = RepoInfo
  { riDirectory :: !RepoDirectory
  , riName :: !RepoName
  , riUrl :: !RepoUrl
  , riGitHash :: !GitHash
  , riNixSha :: !(Maybe NixSha) -- Stack.yaml does not contain this field.
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
  | TextNixSha !Text
  deriving (Eq, Show)


gitNameFromUrl :: RepoUrl -> RepoName
gitNameFromUrl =
      RepoName
    . Text.pack
    . takeBaseName
    . dropTrailingPathSeparator
    . Text.unpack
    . unRepoUrl

repoMapify :: [RepoInfo] -> RepoInfoMap
repoMapify =
  Map.fromList . map (\ ri -> (riName ri, ri))

repoMapFilter :: RepoInfoMap -> [RepoName] -> RepoInfoMap
repoMapFilter rmap _names = rmap
  -- Map.filterWithKey (\ k _ -> k `elem` names) rmap
