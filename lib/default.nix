{ lib, collective-lib, nix-parsec, ...}:

rec {
  parser = import ./parser/default.nix {
    inherit lib;
    collective-lib = baseLib;
    inherit nix-parsec;
  };

  eval = import ./eval/default.nix {
    inherit lib;
    collective-lib = baseLib;
    parser = parser;
  };

  debuglib = import ./debuglib.nix {
    inherit lib;
    collective-lib = baseLib;
  };
}
