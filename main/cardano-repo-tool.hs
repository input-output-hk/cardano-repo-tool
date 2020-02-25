{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (unless)
import           Control.Exception (SomeException, throwIO, catch)

import           Data.Monoid ((<>))

import qualified Data.Text.IO as Text

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt

import           RepoTool (RepoDirectory (..), gitCloneRepo, gitPullRebase, gitRepoStatuses,
                    gitResetChanges, printRepoName, renderRepoHash, updateAllRepoGitHashes,
                    updateCabalFromStack, updateRepoGitHash, updateStackFromCabal)

import           System.Directory (doesDirectoryExist)
import           System.Environment (getProgName)
import           System.Exit (exitFailure)


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
  = CmdCloneRepos
  | CmdPrintGitHashes
  | CmdListRepos
  | CmdRepoStatus
  | CmdResetChanges
  | CmdUpdateGitHash RepoDirectory
  | CmdUpdateGitHashes
  | CmdUpdateGitRepo RepoDirectory
  | CmdUpdateGitRepos
  | CmdUpdateStackFromCabal
  | CmdUpdateCabalFromStack

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
    ( Opt.command "clone-repos"
       ( Opt.info (pure CmdCloneRepos)
       $ Opt.progDesc "Clone any missing repos into the current directory."
       )
    <> Opt.command "print-hashes"
       ( Opt.info (pure CmdPrintGitHashes)
       $ Opt.progDesc "Print the git hashes for the relevant repos."
       )
    <> Opt.command "list-repos"
       ( Opt.info (pure CmdListRepos)
       $ Opt.progDesc "List the repos expected by this tool."
       )
    <> Opt.command "repo-status"
       ( Opt.info (pure CmdRepoStatus)
       $ Opt.progDesc "List the statuses of each repo."
       )
    <> Opt.command "reset-changes"
       ( Opt.info (pure CmdResetChanges)
       $ Opt.progDesc "Reset any changes to the cabal.project and stack.yaml files."
       )
    <> Opt.command "update-hash"
       ( Opt.info pUpdateGitHash
       $ Opt.progDesc "Get the latest git hashes, and update the stack.yaml and cabal.project file for the specified repo."
       )
    <> Opt.command "update-hashes"
       ( Opt.info (pure CmdUpdateGitHashes)
       $ Opt.progDesc "Get the latest git hashes, and update all stack.yaml and cabal.project files."
       )
    <> Opt.command "update-repo"
       ( Opt.info (CmdUpdateGitRepo <$> pRepoDirectory)
       $ Opt.progDesc "Update a single repo ('git checkout master && git pull --rebase')."
       )
    <> Opt.command "update-repos"
       ( Opt.info (pure CmdUpdateGitRepos)
       $ Opt.progDesc "Update all repos ('git checkout master && git pull --rebase')."
       )
    <> Opt.command "update-stack-yaml"
       ( Opt.info (pure CmdUpdateStackFromCabal)
       $ Opt.progDesc "Update git hashes in stack.yaml file (in the current directory) from the cabal.project file."
       )
    <> Opt.command "update-cabal-project"
       ( Opt.info (pure CmdUpdateCabalFromStack)
       $ Opt.progDesc "Update git hashes in cabal.project file (in the current directory) from the stack.yaml file."
       )
    )

pUpdateGitHash :: Parser Command
pUpdateGitHash =
  CmdUpdateGitHash <$> pRepoDirectory

pRepoDirectory :: Parser RepoDirectory
pRepoDirectory =
  RepoDirectory <$>
    Opt.strOption
      (  Opt.long "repo"
      <> Opt.short 'r'
      <> Opt.help "A specific repo."
      )

-- -----------------------------------------------------------------------------

runRepoTool :: Command -> IO ()
runRepoTool cmd =
  case cmd of
    CmdCloneRepos -> cloneRepos
    CmdPrintGitHashes -> validateRepos >> mapM_ (\ r -> Text.putStrLn =<< renderRepoHash r) repos
    CmdListRepos -> listRepos
    CmdRepoStatus -> validateRepos >> gitRepoStatuses repos
    CmdResetChanges -> validateRepos >> gitResetChanges repos
    CmdUpdateGitHash repo -> validateRepos >> updateRepoGitHash repos repo
    CmdUpdateGitHashes -> validateRepos >> updateAllRepoGitHashes repos
    CmdUpdateGitRepo repo -> validateRepos >> gitPullRebase repo
    CmdUpdateGitRepos -> updateGitRepos
    CmdUpdateStackFromCabal -> updateStackFromCabal
    CmdUpdateCabalFromStack -> updateCabalFromStack

updateGitRepos :: IO ()
updateGitRepos = do
    validateRepos
    mapM_ (\r -> gitPullRebase r `catch` handler r) repos
  where
    handler :: RepoDirectory -> SomeException -> IO a
    handler (RepoDirectory fpath) e = do
      printRepoName fpath
      putStr ": "
      throwIO e

cloneRepos :: IO ()
cloneRepos =
  mapM_ cloneIfNeeded repos
 where
  cloneIfNeeded rd@(RepoDirectory fpath) = do
    e <- doesDirectoryExist fpath
    unless e $
      gitCloneRepo rd

listRepos :: IO ()
listRepos = do
  putStrLn "Expect the following repos:\n"
  mapM_ (\ r -> putStrLn $ "  " ++ unRepoDirectory r) repos
  putStrLn ""

validateRepos :: IO ()
validateRepos = do
  mapM_ exists repos
 where
  exists (RepoDirectory fpath) = do
    e <- doesDirectoryExist fpath
    unless e $ do
      progName <- getProgName
      mapM_ putStrLn $
        [ "Error:"
        , "  Git repository " ++ fpath ++ " does not exit in the current directory."
        , "  It should be possible to clone it using the command:"
        , "    git clone https://github.com/input-output-hk/"
        , "  Alternatively, you could just run:"
        , "    " ++ progName ++ " clone-repos"
        , "  which would clone repos as needed."
        , ""
        ]
      exitFailure

-- -----------------------------------------------------------------------------

repos :: [RepoDirectory]
repos =
  map RepoDirectory
    [ "goblins"
    , "cardano-base"
    , "cardano-byron-proxy"
    , "cardano-explorer"
    , "cardano-crypto"
    , "cardano-haskell"
    , "cardano-ledger"
    , "cardano-ledger-specs"
    , "cardano-node"
    , "cardano-prelude"
    , "cardano-shell"
    , "cardano-sl-x509"
    , "cardano-wallet"
    , "iohk-monitoring-framework"
    , "ouroboros-network"
    ]

