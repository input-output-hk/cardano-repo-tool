let
  inherit (import ./. {}) pkgs nix-tools;
in
nix-tools._raw.shellFor {
  packages    = ps: with ps; [ cardano-repo-tool ];
  buildInputs = (with nix-tools._raw; [
    cabal-install.components.exes.cabal
    ghcid.components.exes.ghcid
  ]);
}
