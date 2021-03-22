{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git
  ( getGitBranch
  , getGitHash
  , getRepoInfo
  , gitCloneRepo
  , gitPullRebase
  , gitRepoStatuses
  , gitResetChanges
  , printRepoName
  , renderRepoHash
  , updateRepoGitHash
  , updateAllRepoGitHashes
  , updateGitRepo
  ) where

import           Control.Monad (void, when)

import           Data.Char (isHexDigit)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Git.Internal
import           RepoTool.Git.PullRebase
import           RepoTool.Git.Status
import           RepoTool.RepoInfo
import           RepoTool.Types
import           RepoTool.Text
import           RepoTool.UpdateHash

import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.Process (callProcess)


getGitBranch :: RepoDirectory -> IO String
getGitBranch (RepoDirectory fpath) = do
  xs <- lines <$> readProcess gitBinary [ "-C", fpath, "branch" ]
  case filter ("*" `List.isPrefixOf`) xs of
    [] -> pure "master"
    (str:_) -> pure $ List.drop 2 str

getGitHash :: RepoDirectory -> IO GitHash
getGitHash rd@(RepoDirectory fpath) = do
  branch <- getGitBranch rd
  hash <- takeWhile isHexDigit <$>
            readProcess gitBinary [ "-C", fpath, "rev-parse", branch ]
  if length hash == 40
    then pure $ GitHash (Text.pack hash)
    else error $ "getGitHash: Failed for " ++ fpath

getRepoInfo :: RepoDirectory -> IO RepoInfo
getRepoInfo rd = do
  gh <- getGitHash rd
  url <- getRepoUrl rd
  let name = gitNameFromUrl url
  pure $ RepoInfo
            name
            (if null (unRepoDirectory rd)
                then RepoDirectory (Text.unpack $ unRepoName name)
                else rd
                )
            url gh Nothing

getRepoUrl :: RepoDirectory -> IO RepoUrl
getRepoUrl (RepoDirectory fpath) = do
  parts <- splitIntoParts <$> Text.readFile (fpath </> ".git" </> "config")
  case [ p | p@(TextGitRepo _) <- parts] of
    [] -> error $ "getGitUrl: No url found for repo '" ++ fpath ++ "'"
    (TextGitRepo x:_) -> pure $ RepoUrl x
    _ -> error $ "getRepoUrl: impossible"

gitCheckoutMaster :: RepoDirectory -> IO ()
gitCheckoutMaster (RepoDirectory repo) =
  void $ readProcess gitBinary ["-C", repo, "checkout", "master"]

gitCloneRepo :: RepoDirectory -> IO ()
gitCloneRepo (RepoDirectory fpath) =
  callProcess gitBinary [ "clone", "https://github.com/input-output-hk/" ++ fpath ]

gitRepoStatuses :: [RepoDirectory] -> IO ()
gitRepoStatuses =
  mapM_ gitRepoStatus

gitResetChanges :: [RepoDirectory] -> IO ()
gitResetChanges =
  mapM_ resetChanges
 where
  resetChanges (RepoDirectory repo) = do
    ec <- doesFileExist $ repo </> "cabal.project"
    when ec $
      callProcess gitBinary ["-C", repo, "checkout", "cabal.project"]
    es <- doesFileExist $ repo </> "stack.yaml"
    when es $
      callProcess gitBinary ["-C", repo, "checkout", "stack.yaml"]

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
updateGitRepo rd@(RepoDirectory fpath) = do
  gitCheckoutMaster rd
  callProcess gitBinary [ "-C", fpath, "pull", "--rebase" ]
