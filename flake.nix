{
  description = "Crafting Interpreters (Robert Nystrom) companion code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages914 = pkgs.haskell.packages.ghc914;
      haskellPackages912 = pkgs.haskell.packages.ghc912;
    in
    {
      formatter.${system} = pkgs.nixfmt;
      packages.${system}.default = haskellPackages914.callPackage ./hlox {};
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          haskellPackages912.cabal-install
          haskellPackages912.cabal2nix
          haskellPackages912.ghcid
          haskellPackages912.hlint
          haskellPackages912.ormolu
        ];
        inputsFrom = [
          self.packages.${system}.default
        ];
      };
    };
}
