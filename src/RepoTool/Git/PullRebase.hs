module RepoTool.Git.PullRebase
  ( gitPullRebase
  ) where

import           Data.List (isPrefixOf)

import           RepoTool.Git.Internal
import           RepoTool.Types

import           System.Process (readProcess)


gitPullRebase :: RepoDirectory -> IO ()
gitPullRebase (RepoDirectory fpath) = do
  xs <- filter lineFilter . lines <$> readProcess gitBinary [ "-C", fpath, "pull", "--rebase" ] ""
  if null xs
    then printRepoNameOk fpath
    else do
      putStrLn ""
      printRepoName fpath
      putStrLn ":"
      mapM_ putStrLn xs
      putStrLn ""

lineFilter :: String -> Bool
lineFilter l
  | null l = False
  | "Your branch is up to date" `isPrefixOf` l = False
  | "Unpacking objects" `isPrefixOf` l = False
  | "Already up to date" `isPrefixOf` l = False
  | "Current branch " `isPrefixOf` l = False
  | otherwise = True
