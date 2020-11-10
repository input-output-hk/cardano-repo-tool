{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateHash
  ( extractRepoNames
  , updateHashes
  , updateUrlHashes
  ) where

import           Control.Exception (IOException, throwIO)
import qualified Control.Exception as Expection

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Nix
import           RepoTool.RepoInfo
import           RepoTool.Text
import           RepoTool.Types

import           System.FilePath ((</>))
import           System.IO.Error


updateHashes :: RepoDirectory -> ConfigType -> RepoInfoMap -> IO ()
updateHashes rd cfgType rmapOrig = do
  let configFile = getConfigFile rd cfgType
  putStrLn $ "Updating " ++ configFile ++ " ..."
  result <- Expection.try (Text.readFile configFile)
  case result of
    Left (ioe :: IOException)
      | isDoesNotExistError ioe -> pure ()   -- File didn't exist. Nothing to do.
      | otherwise -> throwIO ioe
    Right before -> do
      let parts = splitIntoParts before
      rmap <- case cfgType of
                ConfigCabalProject ->
                  getAllNixShas $ repoMapFilter rmapOrig (extractRepoNames parts)
                ConfigStackYaml ->
                  pure rmapOrig

      let after = concatParts $ updateUrlHashes rmap parts
      if (after /= before)
        then do putStrLn $ "Updated  " ++ configFile
                Text.writeFile configFile after
        else putStrLn $ "No changes to " ++ configFile

updateUrlHashes :: RepoInfoMap -> [TextPart] -> [TextPart]
updateUrlHashes rmap =
  snd . List.mapAccumL func Nothing
 where
  func :: Maybe RepoInfo -> TextPart -> (Maybe RepoInfo, TextPart)
  func mrepo tp =
    case tp of
      TextWhitespace _ -> (mrepo, tp)
      TextReadable _ -> (mrepo, tp)
      TextGitRepo txt -> (Map.lookup (gitNameFromUrl $ RepoUrl txt) rmap, updateGitHash rmap (RepoUrl txt))
      TextGitHash _ -> (mrepo, maybe tp (TextGitHash . unGitHash . riGitHash) mrepo)
      TextNixSha _ -> (mrepo, maybe tp (maybe tp (TextNixSha . unNixSha) . riNixSha) mrepo)

updateGitHash :: RepoInfoMap -> RepoUrl -> TextPart
updateGitHash rmap (RepoUrl txt) = do
  let repoNames = map unRepoName $ Map.keys rmap
      parts = Text.splitOn "/" txt
      hasHash = any isGitHash parts
  if not hasHash
    then TextGitRepo txt
    else do
      case filter (`elem` repoNames) parts of
        [] -> TextGitRepo txt
        (name:_) ->
          case Map.lookup (RepoName name) rmap of
            Nothing -> TextGitRepo txt
            Just ri -> TextGitRepo $ Text.intercalate "/" (map (replaceGitHash ri) parts)

 where
   replaceGitHash :: RepoInfo -> Text -> Text
   replaceGitHash ri tp =
     if isGitHash tp
       then unGitHash $ riGitHash ri
       else tp

extractRepoNames :: [TextPart] -> [RepoName]
extractRepoNames =
    List.nub . List.sort . mapMaybe getRepoName
  where
    getRepoName :: TextPart -> Maybe RepoName
    getRepoName tp =
      case tp of
        TextGitRepo url -> Just $ gitNameFromUrl (RepoUrl url)
        _otherwise -> Nothing

getConfigFile :: RepoDirectory -> ConfigType -> FilePath
getConfigFile (RepoDirectory fpath) cfgFile =
  case cfgFile of
    ConfigCabalProject -> fpath </> "cabal.project"
    ConfigStackYaml -> fpath </> "stack.yaml"
