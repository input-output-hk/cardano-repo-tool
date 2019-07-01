module RepoTool.Cabal
  ( updateCabalProject
  ) where

import           RepoTool.Types

updateCabalProject :: RepoDirectory -> [(RepoDirectory, GitHash)] -> IO ()
updateCabalProject _ _ = putStrLn "updateCabalProject: Not implemented yet"
