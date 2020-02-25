{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateCabal
  ( extractRepoInfo
  , updateCabalFromStack
  ) where

import qualified Data.List as List
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Text
import           RepoTool.Types
import           RepoTool.UpdateHash

-- | Read all git repository/hash pairs from the stack.yaml file and apply
-- them to the cabal.project file in the same directory.
updateCabalFromStack :: IO ()
updateCabalFromStack = do
  stackParts <- splitIntoParts <$> Text.readFile "stack.yaml"
  cabalParts <- splitIntoParts <$> Text.readFile "cabal.project"
  Text.writeFile "cabal.project" $ do
    let repoNames = extractRepoNames cabalParts
    concatParts (updateUrlHashes (repoMapFilter (extractRepoInfo stackParts) repoNames) cabalParts)

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
      case mr of
        Nothing -> Nothing
        Just repo ->
          let name = gitNameFromUrl repo in
          Just $ RepoInfo (RepoDirectory "") name repo
                    (fromMaybe (errorName name) mg)
                    mn

    errorName :: RepoName -> a
    errorName (RepoName name) =
      error $ "Not able to find git hash for repo " ++ Text.unpack name

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
