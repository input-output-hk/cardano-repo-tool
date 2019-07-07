{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateCabal
  ( updateCabalFromStack
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
  Text.writeFile "cabal.project" $
    concatParts (updateUrlHashes (extractRepoInfo stackParts) cabalParts)

-- -----------------------------------------------------------------------------

extractRepoInfo :: [TextPart] -> RepoInfoMap
extractRepoInfo =
  Map.fromList . map (\ri -> (riName ri, ri)) . snd . List.foldl' func (Nothing, [])
 where
  func :: (Maybe RepoUrl, [RepoInfo]) -> TextPart -> (Maybe RepoUrl, [RepoInfo])
  func (mt, ris) tp =
    case tp of
      TextWhitespace _ -> (mt, ris)
      TextReadable _ -> (mt, ris)
      TextGitHash hash -> maybe (mt, ris) (update ris (GitHash hash)) mt
      TextGitRepo repo -> (Just $ RepoUrl repo, ris)

  update :: [RepoInfo] -> GitHash -> RepoUrl -> (Maybe a, [RepoInfo])
  update ris hash repo =
    ( Nothing
    , RepoInfo (RepoDirectory "") hash (gitNameFromUrl repo) repo : ris
    )
