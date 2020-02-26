############################################################################
# cardano-repo-tool Nix build
#
# nix build -f default.nix -A cardano-repo-tool
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, overlays ? []
# nixpkgs-19.09-darwin as of Jan 16th 2020
, nixpkgs ? import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/f69a5b2.tar.gz)

# haskell.nix as of Feb 26th 2020
, haskell-nix ? import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/21c5528.tar.gz)

# pkgs is nixpkgs with the haskell-nix as agument. But we'll extend haskell-nix to allow adding additional overlays and config values.
, pkgs ? nixpkgs (haskell-nix // {
    inherit system crossSystem;
    overlays = (haskell-nix.overlays or []) ++ overlays;
    config = (haskell-nix.config or {}) // config;
  })
}:
# for CI to build all attributes, we need to recurse into them; so we'll use this helper
let recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x; in
let haskellPackages = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
  # we are only intersted in listing the project packages
  (pkgs.haskell-nix.haskellLib.selectProjectPackages
    # from our project which is based on a cabal project.
    (pkgs.haskell-nix.cabalProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    }));
in {
  # Grab the executable component of our package.
  inherit (haskellPackages.cardano-repo-tool.components.exes) cardano-repo-tool;
}