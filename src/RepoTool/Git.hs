{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git
  ( getGitHash
  , getRepoInfo
  , renderRepoHash
  , updateGitHashes
  , updateGitRepo
  ) where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Cabal
import           RepoTool.Stack
import           RepoTool.Types
import           RepoTool.Text

import           System.FilePath ((</>))
import           System.Process (callProcess, readProcess)


getGitHash :: RepoDirectory -> IO GitHash
getGitHash (RepoDirectory fpath) = do
  hash <- takeWhile isHexDigit <$>
            readProcess gitBinary [ "-C", fpath, "rev-parse", "master" ] ""
  if length hash == 40
    then pure $ GitHash (Text.pack hash)
    else error $ "getGitHash: Failed for " ++ fpath

getRepoInfo :: RepoDirectory -> IO RepoInfo
getRepoInfo rd = do
  gh <- getGitHash rd
  url <- getRepoUrl rd
  pure $ RepoInfo rd gh (gitNameFromUrl url) url

getRepoUrl :: RepoDirectory -> IO RepoUrl
getRepoUrl (RepoDirectory fpath) = do
  parts <- splitIntoParts <$> Text.readFile (fpath </> ".git" </> "config")
  case [ p | p@(TextGitRepo _) <- parts] of
    [] -> error $ "getGitUrl: No url found for repo '" ++ fpath ++ "'"
    (TextGitRepo x:_) -> pure $ RepoUrl x
    _ -> error $ "getGitUrl: impossible"

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
  rmap <- repoMapify <$> mapM getRepoInfo repos
  mapM_ (updateRepo rmap) repos
 where
  updateRepo :: RepoInfoMap -> RepoDirectory -> IO ()
  updateRepo rmap rd = do
    updateStackYaml rd rmap
    updateCabalProject rd rmap

updateGitRepo :: RepoDirectory -> IO ()
updateGitRepo (RepoDirectory fpath) = do
  callProcess gitBinary [ "-C", fpath, "checkout", "master" ]
  callProcess gitBinary [ "-C", fpath, "pull", "--rebase" ]

gitBinary :: String
gitBinary = "/usr/bin/git"
