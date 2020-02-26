{
  extras = hackage:
    {
      packages = {} // {
        cardano-repo-tool = ./cardano-repo-tool.nix;
        nix-archive = ./nix-archive.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }