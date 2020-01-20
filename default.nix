############################################################################
# cardano-repo-tool Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sources ? import ./nix/sources.nix
# Use pinned Nixpkgs with Haskell.nix overlay
, pkgs ? import ./nix/nixpkgs-haskell.nix  { inherit system crossSystem config sources; }
# Import IOHK common nix lib
, iohkLib ? import sources.iohk-nix {
    inherit system crossSystem config;
    sourcesOverride = {
      inherit (sources) nixpkgs;
    };
  }
# Use this git revision for stamping executables
, gitrev ? iohkLib.commitIdFromGitRepoOrZero ./.git
}:
#
# The default.nix file. This will generate targets for all
# buildables (see release.nix for nomenclature, excluding
# the "build machine" last part, specific to release.nix), eg.:
#
# Generated targets include anything from stack.yaml
# (via nix-tools:stack-to-nix and the nix/regenerate.sh script)
# or cabal.project (via nix-tools:plan-to-nix), including all
# version overrides specified there.
#
# Nix-tools stack-to-nix will generate the `nix/.stack-pkgs.nix`
# file which is imported from the `nix/pkgs.nix` where further
# customizations outside of the ones in stack.yaml/cabal.project
# can be specified as needed for nix/ci.
#
# Please run `nix/regenerate.sh` after modifying stack.yaml
# or relevant part of cabal configuration files.
# When switching to recent stackage or hackage package version,
# you might also need to update the iohk-nix common lib. You
# can do so by running the `nix/update-iohk-nix.sh` script.
#
# More information about iohk-nix and nix-tools is available at:
# https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix-toolification.org#for-a-stackage-project
#


# We will need to import the iohk-nix common lib, which includes
# the nix-tools tooling.
let
  src = pkgs.haskell-nix.cleanSourceHaskell {
    src = ./.;
    name = "cardano-repo-tool";
  };
  project = pkgs.haskell-nix.cabalProject' {
    inherit src;
  };
  haskellPackages = project.hsPkgs;
  niv = (import sources.niv {}).niv;
in {
  inherit pkgs iohkLib src haskellPackages niv;
  inherit (iohkLib) nix-tools;
  inherit (haskellPackages.cardano-repo-tool.identifier) version;

  shell = haskellPackages.shellFor {
    name = "cardano-repo-tool";
    packages = ps: with ps; [
      cardano-repo-tool
    ];
    buildInputs = ([
      (pkgs.haskell-nix.hackage-package {
         name = "ghcid"; version = "0.8.1"; }).components.exes.ghcid
      (pkgs.haskell-nix.hackage-package {
         name = "hlint"; version = "2.2.7"; }).components.exes.hlint
      (pkgs.haskell-nix.hackage-package {
         name = "cabal-install"; version = "3.0.0.0"; modules = [{reinstallableLibGhc = true;}]; }).components.exes.cabal
    ]) ++ [
      niv
      # pkgs.cabal-install
    ];
    meta.platforms = pkgs.lib.platforms.unix;
  };
}
