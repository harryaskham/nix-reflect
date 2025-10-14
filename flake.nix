{
  description = "Nix library for parsing, evaluating and reflecting on Nix code, written in pure Nix";

  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    #flake-utils.url = "github:numtide/flake-utils";
    #nix-parsec.url = "github:nprindle/nix-parsec";
    collective-public = {
      url = "git+ssh://git@github.com/harryaskham/collective-public";
      inputs.nix-reflect.follows = "";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "git+https://git@github.com/nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs-stable.url = "git+https://git@github.com/nixos/nixpkgs?ref=release-25.05";
    nix-parsec.url = "git+https://git@github.com/nprindle/nix-parsec";
  };

  outputs = { self, nixpkgs, flake-utils, collective-public, nix-parsec, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        reflectLib = import ./lib { inherit inputs pkgs; };
      in {
        lib = reflectLib;
        devShells = rec {
          default = pkgs.mkShell (rec {
            buildInputs = with pkgs; [ direnv ];
          });
        };
      }
    ) // {
      inherit inputs;
    };
}
