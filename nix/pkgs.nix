{ pkgs
, src
, iohk-extras ? {}
, iohk-module ? {}
}:
let
  haskell = pkgs.haskell-nix;

   # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras haskell.hackage).compiler.nix-name;
  fetchHackage = pname: version: sha256: pkgs.fetchurl { url = "mirror://hackage/${pname}-${version}.tar.gz"; inherit sha256; };
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-extras = [
      (hackage: {
        packages = {
          Cabal = (((hackage.Cabal)."3.0.0.0").revisions).default;
          cabal-install = (((hackage.cabal-install)."3.0.0.0").revisions).default;
        };
      })
      # stack-pkgs.extras
      # iohk-extras.${compiler}
    ];
    modules = [
      { reinstallableLibGhc = true; }
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module

       {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
         # packages.cardano-repo-tool.components.library.buildable = false;
      }
    ];
  };

 in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
