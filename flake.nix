{
  description = "Nix library for parsing, evaluating and reflecting on Nix code, written in pure Nix";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-parsec.url = "github:nprindle/nix-parsec";
    collective-public = {
      url = "git+ssh://git@github.com:harryaskham/collective-public?ref=dev";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-reflect = inputs.self;
    };
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
