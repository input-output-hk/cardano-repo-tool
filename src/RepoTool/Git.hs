module RepoTool.Git where

import           Data.Char (isHexDigit)
import           Data.Text (Text)
import qualified Data.Text as Text

import           System.Directory (withCurrentDirectory)
import           System.Process (readProcess)


newtype GitHash
  = GitHash { unGitHash :: Text }
  deriving (Eq, Show)

getGitHash :: FilePath -> IO GitHash
getGitHash fpath = do
  hash <- takeWhile isHexDigit <$> withCurrentDirectory fpath (readProcess gitBinary [ "rev-parse", "master" ] "")
  if length hash == 40
    then pure $ GitHash (Text.pack hash)
    else error $ "getGitHash: Failed for " ++ fpath

gitBinary :: String
gitBinary = "/usr/bin/git"
