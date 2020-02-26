{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "nix-archive"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "A library and executable for manipulating Nix Archive files";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.attoparsec)
          (hsPkgs.base16-bytestring)
          (hsPkgs.base32-z-bytestring)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.memory)
          (hsPkgs.pretty-show)
          (hsPkgs.process-extras)
          (hsPkgs.quiet)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.transformers-except)
          ];
        };
      exes = {
        "nar" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.filepath)
            (hsPkgs.nix-archive)
            (hsPkgs.optparse-applicative)
            (hsPkgs.text)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/nix-archive";
      rev = "6ec16124fa060818b2dbd4b6e99b0ce4906c59e6";
      sha256 = "0waldaw2yz9hhdlk8f8gmd37z4cgl5p3i3v6np1h9ydz4jnbbfm1";
      });
    }