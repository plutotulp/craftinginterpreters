{
  description = "Crafting Interpreters (Robert Nystrom) companion code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      pkgsFor =
        system:
        import nixpkgs {
          localSystem.system = system;
          overlays = [ ];
        };
    in
    flake-utils.lib.eachSystem systems (
      system:
      let
        pkgs = pkgsFor system;
        haskellPackages914 = pkgs.haskell.packages.ghc914;
        haskellPackages912 = pkgs.haskell.packages.ghc912;
      in
      {
        formatter = pkgs.nixfmt;
        packages.default = haskellPackages914.callPackage ./hlox { };
        devShells.default = pkgs.mkShell {
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
      }
    );
}
