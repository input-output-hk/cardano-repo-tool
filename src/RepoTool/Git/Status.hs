{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git.Status
  ( gitRepoStatus
  ) where

import           Data.List (isPrefixOf)

import           RepoTool.Git.Internal
import           RepoTool.Types


gitRepoStatus :: RepoDirectory -> IO ()
gitRepoStatus (RepoDirectory fpath) = do
  xs <- filter lineFilter . lines <$> readProcess gitBinary [ "-C", fpath, "status" ]
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
  | "On branch master" == l = False
  | "Your branch is up to date" `isPrefixOf` l = False
  | "nothing to commit" `isPrefixOf` l = False
  | otherwise = True
