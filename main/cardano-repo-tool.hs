{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           RepoTool (GitHash (..), getGitHash)


main :: IO ()
main =
  mapM_ printGitHashes repos



printGitHashes :: FilePath -> IO ()
printGitHashes repo = do
  gh <- getGitHash repo
  Text.putStrLn $ mconcat
    [ Text.pack repo
    , Text.replicate (30 - length repo) " "
    , unGitHash gh
    ]


repos :: [FilePath]
repos =
  [ "cardano-base"
  , "cardano-crypto"
  , "cardano-ledger"
  , "cardano-ledger-specs"
  , "cardano-node"
  , "cardano-prelude"
  , "cardano-shell"
  , "cardano-sl-x509"
  , "iohk-monitoring-framework"
  , "ouroboros-network"
  ]

