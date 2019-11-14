{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RepoTool.UpdateStack
  ( updateStackFromCabal
  ) where

import qualified Data.Text.IO as Text

import           RepoTool.Text
import           RepoTool.UpdateHash
import           RepoTool.UpdateCabal

-- | Read all git repository/hash pairs from the stack.yaml file and apply
-- them to the cabal.project file in the same directory.
updateStackFromCabal :: IO ()
updateStackFromCabal = do
  cabalParts <- splitIntoParts <$> Text.readFile "cabal.project"
  stackParts <- splitIntoParts <$> Text.readFile "stack.yaml"
  Text.writeFile "stack.yaml" $
    concatParts (updateUrlHashes (extractRepoInfo cabalParts) stackParts)
