{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.Cabal
  ( updateCabalProject
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


updateCabalProject :: RepoDirectory -> RepoInfoMap -> IO ()
updateCabalProject (RepoDirectory fpath) rmap = do
  let cabalProject = fpath </> "cabal.project"
  result <- Expection.try $ Text.readFile cabalProject
  case result of
    Left (_ :: IOException) -> pure ()   -- File didn't exist. Nothing to do.
    Right before -> do
      let after = concatParts $ updateHashes rmap (splitIntoParts before)
      when (after /= before) $ do
        putStrLn $ "Updated " ++ cabalProject
        Text.writeFile cabalProject after

updateHashes :: RepoInfoMap -> [TextPart] -> [TextPart]
updateHashes rmap =
  snd . List.mapAccumL func Nothing
 where
  func :: Maybe RepoInfo -> TextPart -> (Maybe RepoInfo, TextPart)
  func mrepo tp =
    case tp of
      TextWhitespace _ -> (mrepo, tp)
      TextReadable _ -> (mrepo, tp)
      TextGitRepo txt -> (Map.lookup (gitNameFromUrl $ RepoUrl txt) rmap, tp)
      TextGitHash _ -> (Nothing, maybe tp (TextGitHash . unGitHash . riHash) mrepo)
