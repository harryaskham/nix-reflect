{
  description = "Nix library for parsing, evaluating and reflecting on Nix code, written in pure Nix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    collective-public.url = "github:harryaskham/collective-public";
    nix-parsec.url = "github:nprindle/nix-parsec";
  };

  outputs = { self, nixpkgs, flake-utils, collective-public, nix-parsec, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        collective-lib = collective-public.packages.${system}.collective-lib;
        reflectLib = import ./lib {
          inherit (pkgs) lib;
          inherit collective-lib nix-parsec;
        };
      in {
        lib = reflectLib;
        devShells.default = pkgs.mkShell { };  
      }
    ) // {
      inherit inputs;
    };
}
