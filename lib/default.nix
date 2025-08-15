{ pkgs, inputs, ...}:

let
  lib = pkgs.lib;
  collective-lib = inputs.collective-public.lib.${pkgs.system};
  nix-parsec = inputs.nix-parsec;
in rec {
  parser = import ./parser/default.nix {
    inherit lib collective-lib nix-parsec;
  };

  eval = import ./eval/default.nix {
    inherit lib collective-lib parser;
  };

  debuglib = import ./debuglib.nix {
    inherit lib collective-lib;
  };
}
