{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateHash
  ( updateHashes
  ) where

import           Control.Exception (IOException)
import qualified Control.Exception as Expection
import           Control.Monad (when)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
      TextGitRepo txt -> (Map.lookup (gitNameFromUrl $ RepoUrl txt) rmap, tp)
      TextGitHash _ -> (Nothing, maybe tp (TextGitHash . unGitHash . riHash) mrepo)

getConfigFile :: RepoDirectory -> ConfigType -> FilePath
getConfigFile (RepoDirectory fpath) cfgFile =
  case cfgFile of
    ConfigCabalProject -> fpath </> "cabal.project"
    ConfigStackYaml -> fpath </> "stack.yaml"


