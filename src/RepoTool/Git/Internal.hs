{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Git.Internal
  ( gitBinary
  , printRepoName
  , printRepoNameOk
  , readProcess
  ) where

import           System.Console.ANSI (ConsoleLayer (..), Color (..),
                    ColorIntensity (..), SGR (..), setSGR)
import           System.Exit (ExitCode (..))
import qualified System.Process as Process


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

-- | The 'System.Process.readProcess' function does not affect 'stderr'. This version
-- captures 'stderr' pipes it to the output along with 'stdout.
readProcess
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> IO String                -- ^ output
readProcess cmd args = do
  (code, out, err) <- Process.readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> pure $ err ++ out
    ExitFailure _ -> error $ "RepoTool.Git.Internal.readProcess: " ++ err ++ out
