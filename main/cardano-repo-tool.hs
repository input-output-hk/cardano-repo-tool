{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))

import qualified Data.Text.IO as Text

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt

import           RepoTool (RepoDirectory (..), renderRepoHash, updateGitHashes, updateGitRepo)


main :: IO ()
main =
  Opt.customExecParser p opts >>= runRepoTool
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pCommand)
      ( Opt.fullDesc
      <> Opt.header "cardano-repo-tool - A tool for managing the Cardano repos."
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = CmdPrintGitHashes
  | CmdUpdateGitHashes
  | CmdUpdateGitRepos

-- -----------------------------------------------------------------------------

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption "cabal-repo-tool version 0.1.0.0"
    (  Opt.long "version"
    <> Opt.short 'v'
    <> Opt.help "Print the version and exit"
    )

pCommand :: Parser Command
pCommand =
  Opt.subparser
    ( Opt.command "print-hashes"
       ( Opt.info (pure CmdPrintGitHashes)
       $ Opt.progDesc "Print the git hashes for the relevant repos."
       )
    <> Opt.command "update-hashes"
       ( Opt.info (pure CmdUpdateGitHashes)
       $ Opt.progDesc "Get the latest git hashes, and update all stack.yaml and cabal.project files."
       )
    <> Opt.command "update-repos"
       ( Opt.info (pure CmdUpdateGitRepos)
       $ Opt.progDesc "Run 'git checkout master && git pull --rebase' on all repos."
       )
    )

-- -----------------------------------------------------------------------------

runRepoTool :: Command -> IO ()
runRepoTool cmd =
  case cmd of
    CmdPrintGitHashes -> mapM_ (\ r -> Text.putStrLn =<< renderRepoHash r) repos
    CmdUpdateGitHashes -> updateGitHashes repos
    CmdUpdateGitRepos -> mapM_ updateGitRepo repos

-- -----------------------------------------------------------------------------

repos :: [RepoDirectory]
repos =
  map RepoDirectory
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

