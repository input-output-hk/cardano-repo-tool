{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateHash
  ( updateHashes
  ) where

import           Control.Exception (IOException)
import qualified Control.Exception as Expection
import           Control.Monad (when)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool.Text
import           RepoTool.Types

import           System.FilePath ((</>))


updateHashes :: RepoDirectory -> ConfigType -> RepoInfoMap -> IO ()
updateHashes rd cfgType rmap = do
  let configFile = getConfigFile rd cfgType
  result <- Expection.try $ Text.readFile configFile
  case result of
    Left (_ :: IOException) -> pure ()   -- File didn't exist. Nothing to do.
    Right before -> do
      let after = concatParts . snd $ List.mapAccumL func Nothing (splitIntoParts before)
      when (after /= before) $ do
        putStrLn $ "Updated " ++ configFile
        Text.writeFile configFile after
 where
  func :: Maybe RepoInfo -> TextPart -> (Maybe RepoInfo, TextPart)
  func mrepo tp =
    case tp of
      TextWhitespace _ -> (mrepo, tp)
      TextReadable _ -> (mrepo, tp)
      TextGitRepo txt -> (Map.lookup (gitNameFromUrl $ RepoUrl txt) rmap, updateUrlHash rmap txt)
      TextGitHash _ -> (Nothing, maybe tp (TextGitHash . unGitHash . riHash) mrepo)


updateUrlHash :: RepoInfoMap -> Text -> TextPart
updateUrlHash rmap txt = do
  let repoNames = map unRepoName $ Map.keys rmap
      parts = Text.splitOn "/" txt
      hasHash = any isGitHash parts
  if not hasHash
    then TextGitRepo txt
    else do
      case filter (`elem` repoNames) parts of
        [] -> TextGitRepo txt
        [name] ->
          case Map.lookup (RepoName name) rmap of
            Nothing -> TextGitRepo txt
            Just ri -> TextGitRepo $ Text.intercalate "/" (map (replaceHash ri) parts)
        _ -> error "updateUrlHash: Can this even happen?"

 where
   replaceHash :: RepoInfo -> Text -> Text
   replaceHash ri tp =
     if isGitHash tp
       then unGitHash $ riHash ri
       else tp

getConfigFile :: RepoDirectory -> ConfigType -> FilePath
getConfigFile (RepoDirectory fpath) cfgFile =
  case cfgFile of
    ConfigCabalProject -> fpath </> "cabal.project"
    ConfigStackYaml -> fpath </> "stack.yaml"


