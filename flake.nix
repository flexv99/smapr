# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        myHaskellOverrides = self: super: {
          # Use haskell.lib.dontCheck to disable the test suites
          # Apply it to any packages that have failing tests.
          # dontCheck is not TRANSITIVE!, so you need to list them explicitly.
          network = pkgs.haskell.lib.dontCheck super.network;
          vector-space = super.vector-space_0_19;
        };

        haskellPackages = pkgs.haskell.packages.ghc984.override {
          overrides = myHaskellOverrides;
        };

        jailbreakUnbreak =
          pkg:
          pkgs.haskell.lib.doJailbreak (
            pkg.overrideAttrs (_: {
              meta = { };
            })
          );

        packageName = "smapr";
      in
      {
        packages.${packageName} = (haskellPackages.callCabal2nix packageName self { }).overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.protobuf ];
          buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.zlib ];
        });

        packages.default = self.packages.${system}.${packageName};

        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            ghcid
            cabal-install
            protobuf
            zlib
          ];
        };
        devShell = self.devShells.${system}.default;
      }
    );
}
