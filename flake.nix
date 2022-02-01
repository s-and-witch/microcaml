# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Awesome llvm-based compiler written in haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = self: original: {
            
          };
        };

        project = self.packages.${system}.${packageName};

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "microcaml";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = project;

        devShell = project.env.overrideAttrs
          (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [
              pkgs.haskellPackages.haskell-language-server # you must build it with your ghc to work
              pkgs.cabal-install
            ];
            buildInputs = old.buildInputs ++ [            
              pkgs.llvmPackages_9.llvm
            ];
          }
          );
      });
}
