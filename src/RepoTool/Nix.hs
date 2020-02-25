{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RepoTool.Nix
  ( getNixSha
  , getAllNixShas
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import qualified Nix.Nar as Nar

import           RepoTool.Types


getAllNixShas :: RepoInfoMap -> IO RepoInfoMap
getAllNixShas rmap = do
    Map.fromList <$> mapM update (Map.toList rmap)
  where
    update :: (RepoName, RepoInfo) -> IO (RepoName, RepoInfo)
    update (name, ri) = do
      msha <- getNixSha ri (riGitHash ri)
      pure $ (name, ri { riNixSha = msha})

getNixSha :: RepoInfo -> GitHash -> IO (Maybe NixSha)
getNixSha ri (GitHash ghash) =
  either (const Nothing) (Just . NixSha . Text.decodeUtf8)
    <$> Nar.nixShaGitRepoAtHash
          (unRepoDirectory $ riDirectory ri)
          (Nar.GitHash $ Text.encodeUtf8 ghash)
