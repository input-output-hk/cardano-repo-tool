{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git
  ( getGitHash
  , getRepoInfo
  , gitCloneRepo
  , gitRepoStatuses
  , renderRepoHash
  , updateRepoGitHash
  , updateAllRepoGitHashes
  , updateGitRepo
  ) where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Git.Status
import           RepoTool.Types
import           RepoTool.Text
import           RepoTool.UpdateHash

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
    _ -> error $ "getRepoUrl: impossible"

gitCloneRepo :: RepoDirectory -> IO ()
gitCloneRepo (RepoDirectory fpath) =
  callProcess gitBinary [ "clone", "https://github.com/input-output-hk/" ++ fpath ]

gitRepoStatuses :: [RepoDirectory] -> IO ()
gitRepoStatuses =
  mapM_ gitRepoStatus

renderRepoHash :: RepoDirectory -> IO Text
renderRepoHash rd@(RepoDirectory repo) = do
  gh <- getGitHash rd
  pure $ mconcat
    [ Text.pack repo
    , Text.replicate (30 - length repo) " "
    , unGitHash gh
    ]

updateRepoGitHash :: [RepoDirectory] -> RepoDirectory -> IO ()
updateRepoGitHash repos repo = do
  rmap <- repoMapify <$> mapM getRepoInfo repos
  updateRepoGitHashes rmap repo

updateAllRepoGitHashes :: [RepoDirectory] -> IO ()
updateAllRepoGitHashes repos = do
  rmap <- repoMapify <$> mapM getRepoInfo repos
  mapM_ (updateRepoGitHashes rmap) repos

updateRepoGitHashes :: RepoInfoMap -> RepoDirectory -> IO ()
updateRepoGitHashes rmap rd = do
    updateHashes rd ConfigCabalProject rmap
    updateHashes rd ConfigStackYaml rmap

updateGitRepo :: RepoDirectory -> IO ()
updateGitRepo (RepoDirectory fpath) = do
  callProcess gitBinary [ "-C", fpath, "checkout", "master" ]
  callProcess gitBinary [ "-C", fpath, "pull", "--rebase" ]
