{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateCabal
  ( extractRepoInfo
  , updateCabalFromExternal
  , updateCabalFromStack
  ) where

import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Nix
import           RepoTool.Text
import           RepoTool.Types
import           RepoTool.UpdateHash

import           System.Directory (withCurrentDirectory)

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
  myCabalParts <- splitIntoParts <$> Text.readFile "cabal.project"
  otherCabalParts <- splitIntoParts <$> Text.readFile cabalProject

  let repoNames = extractRepoNames myCabalParts
  repInfo <- withCurrentDirectory ".." $
                    getAllNixShas (extractRepoInfo otherCabalParts)

  Text.writeFile "cabal.project" $
    concatParts (updateUrlHashes (repoMapFilter repInfo repoNames) myCabalParts)


extractRepoInfo :: [TextPart] -> RepoInfoMap
extractRepoInfo =
  repoMapify . mapMaybe foldParts . groupRepos

-- -----------------------------------------------------------------------------

foldParts :: [TextPart] -> Maybe RepoInfo
foldParts =
    convert . List.foldl' folder (Nothing, Nothing, Nothing)
  where
    folder :: (Maybe RepoUrl, Maybe GitHash, Maybe NixSha) -> TextPart -> (Maybe RepoUrl, Maybe GitHash, Maybe NixSha)
    folder (mr, mg, mn) txt =
      case txt of
        TextWhitespace _ -> (mr, mg, mn)
        TextReadable _ -> (mr, mg, mn)
        TextGitRepo repo -> (Just (RepoUrl repo), mg, mn)
        TextGitHash hash -> (mr, Just (GitHash hash), mn)
        TextNixSha hash -> (mr, mg, Just (NixSha hash))

    convert :: (Maybe RepoUrl, Maybe GitHash, Maybe NixSha) -> Maybe RepoInfo
    convert (mr, mg, mn) =
      case (mr, mg) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just repo, Just hash) ->
          let name = gitNameFromUrl repo in
          Just $ RepoInfo (RepoDirectory . Text.unpack $ unRepoName name) name repo hash mn

groupRepos :: [TextPart] -> [[TextPart]]
groupRepos parts =
    case List.break isGitRepo parts of
      -- Drop everything up to the first git repo.
      (_, ys) -> convert ys
  where
    convert :: [TextPart] -> [[TextPart]]
    convert [] = []
    convert (x:xs) =
      case List.break isGitRepo xs of
        (ys, []) -> [ys]
        (ys, zs) -> (x:ys) : convert zs

    isGitRepo :: TextPart -> Bool
    isGitRepo tp =
      case tp of
        TextGitRepo {} -> True
        _otherwise -> False
