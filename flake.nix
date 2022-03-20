{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let

        name = "ede";
        pkgs = nixpkgs.legacyPackages.${system};

      in
      {
        packages.${name} = pkgs.haskellPackages.callCabal2nix name self { };

        defaultPackage = self.packages.${system}.${name};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.cabal-fmt
            nixpkgs-fmt
            ormolu
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };

      });
}
