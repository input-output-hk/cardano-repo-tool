{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateCabal
  ( extractRepoInfo
  , updateCabalFromExternal
  , updateCabalFromStack
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text

import           RepoTool.Git
import           RepoTool.Nix
import           RepoTool.RepoInfo
import           RepoTool.Text
import           RepoTool.Types
import           RepoTool.UpdateHash

import           System.Directory (withCurrentDirectory)
import           System.FilePath (splitPath, takeDirectory)

-- | Read all git repository/hash pairs from the stack.yaml file and apply
-- them to the cabal.project file in the same directory.
updateCabalFromStack :: IO ()
updateCabalFromStack = do
  stackParts <- splitIntoParts <$> Text.readFile "stack.yaml"
  cabalParts <- splitIntoParts <$> Text.readFile "cabal.project"

  let repoNames = extractRepoNames cabalParts
  stackRepInfo <- withCurrentDirectory ".." $
                    getAllNixShas (extractRepoInfo stackParts)

  Text.writeFile "cabal.project" $
    concatParts (updateUrlHashes (repoMapFilter stackRepInfo repoNames) cabalParts)

updateCabalFromExternal :: FilePath -> IO ()
updateCabalFromExternal cabalProject = do
  -- TODO: This should also update the hashes for the project where 'otherCabalParts'
  -- comes from.
  localCabalParts <- splitIntoParts <$> Text.readFile "cabal.project"
  otherCabalParts <- splitIntoParts <$> Text.readFile cabalProject

  otherRepoInfo <- repoInfoFromCabalProject cabalProject

  let otherCabalRepos = repoMapInsert (extractRepoInfo otherCabalParts) otherRepoInfo
      repoNames = Map.keys otherCabalRepos

  repInfo <- withCurrentDirectory ".." $ getAllNixShas otherCabalRepos

  Text.writeFile "cabal.project" $
    concatParts (updateUrlHashes (repoMapFilter repInfo repoNames) localCabalParts)

-- -------------------------------------------------------------------------------------------------

repoInfoFromCabalProject :: FilePath -> IO RepoInfo
repoInfoFromCabalProject fp =
  withCurrentDirectory ".." $ do
    ri <- getRepoInfo (RepoDirectory . List.last . splitPath $ takeDirectory fp)
    msha <- getNixSha ri (riGitHash ri)
    pure $ ri { riNixSha = msha }
