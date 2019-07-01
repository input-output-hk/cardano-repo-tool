{-# LANGUAGE TupleSections #-}

module RepoTool.Git where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text

import           System.Process (callProcess, readProcess)


newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Show)

newtype RepoDirectory
  = RepoDirectory { unRepoDirectory :: FilePath }
  deriving (Eq, Show)

getGitHash :: RepoDirectory -> IO GitHash
getGitHash (RepoDirectory fpath) = do
  hash <- takeWhile isHexDigit <$>
            readProcess gitBinary [ "-C", fpath, "rev-parse", "master" ] ""
  if length hash == 40
    then pure $ GitHash (Text.pack hash)
    else error $ "getGitHash: Failed for " ++ fpath

getRepoHashPair :: RepoDirectory -> IO (RepoDirectory, GitHash)
getRepoHashPair rd =
  (rd,) <$> getGitHash rd

updateGitRepo :: RepoDirectory -> IO ()
updateGitRepo (RepoDirectory fpath) = do
  callProcess gitBinary [ "-C", fpath, "checkout", "master" ]
  callProcess gitBinary [ "-C", fpath, "pull", "--rebase" ]

gitBinary :: String
gitBinary = "/usr/bin/git"
