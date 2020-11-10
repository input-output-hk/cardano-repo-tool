{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath

import           Data.Map.Strict (Map)

import           GHC.Generics
import           Quiet

data ConfigType
  = ConfigCabalProject
  | ConfigStackYaml
  deriving (Eq, Show)

newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Generic)
  deriving Show via (Quiet GitHash)

newtype NixSha
  = NixSha { unNixSha :: Text }
  deriving (Eq, Generic)
  deriving Show via (Quiet NixSha)

newtype RepoDirectory
  = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Generic)
  deriving Show via (Quiet RepoDirectory)

data RepoInfo = RepoInfo
  { riName :: !RepoName
  , riDirectory :: !RepoDirectory
  , riUrl :: !RepoUrl
  , riGitHash :: !GitHash
  , riNixSha :: !(Maybe NixSha) -- Stack.yaml does not contain this field.
  }
  deriving (Eq, Generic)
  deriving Show via (Quiet RepoInfo)

type RepoInfoMap
  = Map RepoName RepoInfo

newtype RepoName
  = RepoName { unRepoName :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet RepoName)

newtype RepoUrl
  = RepoUrl { unRepoUrl :: Text }
  deriving (Eq, Generic)
  deriving Show via (Quiet RepoUrl)

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
