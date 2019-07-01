module RepoTool.Stack
  ( updateStackYaml
  ) where

import qualified Data.Text.IO as Text

import           RepoTool.Text
import           RepoTool.Types

import           System.FilePath ((</>))


updateStackYaml :: RepoDirectory -> [(RepoDirectory, GitHash)] -> IO ()
updateStackYaml (RepoDirectory fpath) _ = do
  sf <- Text.readFile $ fpath </> "stack.yaml"
  putStrLn "updateStackYaml"
  mapM_ print $ splitIntoParts sf


