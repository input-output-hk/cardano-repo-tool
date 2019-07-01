module RepoTool.Git where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text

import           System.Directory (withCurrentDirectory)
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
            withCurrentDirectory fpath
              (readProcess gitBinary [ "rev-parse", "master" ] "")
  if length hash == 40
    then pure $ GitHash (Text.pack hash)
    else error $ "getGitHash: Failed for " ++ fpath

updateGitRepo :: RepoDirectory -> IO ()
updateGitRepo (RepoDirectory fpath) = do
  withCurrentDirectory fpath $ do
    callProcess gitBinary [ "checkout", "master" ]
    callProcess gitBinary [ "pull", "--rebase" ]

gitBinary :: String
gitBinary = "/usr/bin/git"
