############################################################################
# cardano-repo-tool Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

let
  haskell = pkgs.callPackage iohkLib.nix-tools.haskell {};
  src = iohkLib.cleanSourceHaskell ./.;
  util = pkgs.callPackage ./nix/util.nix {};

  # Example of using a package from iohk-nix
  # TODO: Declare packages required by the build.
  inherit (iohkLib.rust-packages.pkgs) jormungandr;

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Pass in any extra programs necessary for the build as function arguments.
    # TODO: Declare packages required by the build.
    # jormungandr and cowsay are just examples and should be removed for your
    # project, unless needed.
    inherit jormungandr;
    inherit (pkgs) cowsay;
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit (haskellPackages.cardano-repo-tool.identifier) version;

  # Grab the executable component of our package.
  inherit (haskellPackages.cardano-repo-tool.components.exes) cardano-repo-tool;

  tests = util.collectComponents "tests" util.isIohkSkeleton haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isIohkSkeleton haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "cardano-repo-tool-shell";
    # TODO: List all local packages in the project.
    packages = ps: with ps; [
      cardano-repo-tool
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ]
      # TODO: Add your own packages to the shell.
      ++ [ jormungandr ];
  };

  # Example of a linting script used by Buildkite.
  checks.lint-fuzz = pkgs.callPackage ./nix/check-lint-fuzz.nix {};

  # Attrset of PDF builds of LaTeX documentation.
  docs = pkgs.callPackage ./docs/default.nix {};
}
