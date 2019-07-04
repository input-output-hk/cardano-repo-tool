{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git.Internal
  ( gitBinary
  , printRepoName
  , printRepoNameOk
  ) where

import           System.Console.ANSI (ConsoleLayer (..), Color (..),
                    ColorIntensity (..), SGR (..), setSGR)

gitBinary :: String
gitBinary = "/usr/bin/git"

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

