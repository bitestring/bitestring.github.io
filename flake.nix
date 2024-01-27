{
  description = "bitestring.github.io";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hs = pkgs.haskell.packages."ghc963"; # need to match Stackage LTS version from stack.yaml resolver
      dependencies = with pkgs; [
        # system deps
        zlib
        pkg-config

        # Haskell deps
        stack
        hs.ghc # GHC compiler in the desired version (will be available on PATH)
        hs.haskell-language-server # LSP server for editor
        hs.ormolu # Haskell formatter
        hs.hlint # Haskell codestyle checker
        hs.hoogle # Lookup Haskell documentation
        hs.implicit-hie # auto generate LSP hie.yaml file from cabal
        hs.retrie # Haskell refactoring tool
      ];

    in {
      # packages.${system}.default = pkgs.hello;

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = dependencies;
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath dependencies;
      };
    };
}
