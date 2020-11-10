module RepoTool.RepoInfo
  ( extractRepoInfo
  , repoMapify
  , repoMapFilter
  , repoMapInsert
  ) where

-- import           Data.Text (Text)
import qualified Data.Text as Text

-- import           Data.Map.Strict (Map)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)

import           RepoTool.Types

extractRepoInfo :: [TextPart] -> RepoInfoMap
extractRepoInfo =
  repoMapify . mapMaybe foldParts . groupRepos

repoMapify :: [RepoInfo] -> RepoInfoMap
repoMapify =
  Map.fromList . map (\ ri -> (riName ri, ri))

repoMapFilter :: RepoInfoMap -> [RepoName] -> RepoInfoMap
repoMapFilter rmap names =
  Map.filterWithKey (\ k _ -> k `elem` names) rmap

repoMapInsert :: RepoInfoMap -> RepoInfo -> RepoInfoMap
repoMapInsert rmap ri = Map.insert (riName ri) ri rmap

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
          Just $ RepoInfo name (RepoDirectory . Text.unpack $ unRepoName name) repo hash mn

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
        (ys, []) -> [x:ys]
        (ys, zs) -> (x:ys) : convert zs

    isGitRepo :: TextPart -> Bool
    isGitRepo tp =
      case tp of
        TextGitRepo {} -> True
        _otherwise -> False
