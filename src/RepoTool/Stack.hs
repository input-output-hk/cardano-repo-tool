module RepoTool.Stack
  ( updateStackYaml
  ) where

import           RepoTool.Types

updateStackYaml :: RepoDirectory -> [(RepoDirectory, GitHash)] -> IO ()
updateStackYaml _ _ = putStrLn "updateStackYaml: Not implemented yet"
