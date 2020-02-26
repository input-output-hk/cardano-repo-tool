{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-repo-tool"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "A tool to update the dependencies in Cardano related git checkouts.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.ansi-terminal)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.nix-archive)
          (hsPkgs.process)
          (hsPkgs.text)
          ];
        };
      exes = {
        "cardano-repo-tool" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-repo-tool)
            (hsPkgs.directory)
            (hsPkgs.optparse-applicative)
            (hsPkgs.text)
            ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-repo-tool)
            (hsPkgs.hedgehog)
            (hsPkgs.text)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }