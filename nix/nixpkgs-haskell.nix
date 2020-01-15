{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sources ? import ./nix/sources.nix
}:

let
  haskellNixArgs = import sources."haskell.nix";

  # Merge config and overlays provided by Haskell.nix into
  # our own nixpkgs args.
  args = haskellNixArgs // {
    inherit system crossSystem;
    config = (haskellNixArgs.config or {}) // config;
    overlays = (haskellNixArgs.overlays or []) ++ overlays;
  };

  overlays = [];
in
  import sources.nixpkgs args
