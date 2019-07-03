{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git.Status
  ( gitBinary
  , gitRepoStatus
  ) where

import           Data.List (isPrefixOf)

import           RepoTool.Types

import           System.Console.ANSI (ConsoleLayer (..), Color (..),
                    ColorIntensity (..), SGR (..), setSGR)
import           System.Process (readProcess)


gitRepoStatus :: RepoDirectory -> IO ()
gitRepoStatus (RepoDirectory fpath) = do
  xs <- filter lineFilter . lines <$> readProcess gitBinary [ "-C", fpath, "status" ] ""
  if null xs
    then printRepoNameOk fpath
    else do
      putStrLn ""
      printRepoName fpath
      putStrLn ":"
      mapM_ putStrLn xs
      putStrLn ""

printRepoName :: String -> IO ()
printRepoName name = do
  setSGR [SetColor Foreground Dull Blue]
  putStr name
  setSGR [Reset]

printRepoNameOk :: String -> IO ()
printRepoNameOk name = do
  printRepoName name
  putStr ": "
  setSGR [SetColor Foreground Dull Green]
  putStr "ok"
  setSGR [Reset]
  putStrLn ""

lineFilter :: String -> Bool
lineFilter l
  | null l = False
  | "On branch master" == l = False
  | "Your branch is up to date" `isPrefixOf` l = False
  | "nothing to commit" `isPrefixOf` l = False
  | otherwise = True

gitBinary :: String
gitBinary = "/usr/bin/git"
