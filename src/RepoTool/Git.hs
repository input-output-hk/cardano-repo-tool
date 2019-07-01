{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git
  ( getGitHash
  , getRepoHashPair
  , renderRepoHash
  , updateGitHashes
  , updateGitRepo
  ) where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text

import           RepoTool.Cabal
import           RepoTool.Stack
import           RepoTool.Types

import           System.Process (callProcess, readProcess)


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

renderRepoHash :: RepoDirectory -> IO Text
renderRepoHash rd@(RepoDirectory repo) = do
  gh <- getGitHash rd
  pure $ mconcat
    [ Text.pack repo
    , Text.replicate (30 - length repo) " "
    , unGitHash gh
    ]

updateGitHashes :: [RepoDirectory] -> IO ()
updateGitHashes repos = do
  pairs <- mapM getRepoHashPair repos
  mapM_ (\ (r, _) -> updateRepo r pairs) pairs
 where
  updateRepo :: RepoDirectory -> [(RepoDirectory, GitHash)] -> IO ()
  updateRepo rd pairs = do
    updateStackYaml rd pairs
    updateCabalProject rd pairs

updateGitRepo :: RepoDirectory -> IO ()
updateGitRepo (RepoDirectory fpath) = do
  callProcess gitBinary [ "-C", fpath, "checkout", "master" ]
  callProcess gitBinary [ "-C", fpath, "pull", "--rebase" ]

gitBinary :: String
gitBinary = "/usr/bin/git"
