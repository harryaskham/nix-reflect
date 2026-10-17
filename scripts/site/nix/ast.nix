lib: with lib; let
  proj = x:
    if parser.isAST x then {
      label = parser.printASTName x;
      type = x.nodeType;
      src = x.__src or null;
      kids = builtins.mapAttrs (_: proj) (parser.hideParams x.__args);
    }
    else if builtins.isList x then { list = map proj x; }
    else if builtins.isAttrs x then { attrs = builtins.mapAttrs (_: proj) (builtins.removeAttrs x ["__type" "__toString" "fmap" "mapNode"]); }
    else if builtins.isFunction x then { fn = true; }
    else { leaf = builtins.toJSON x; };
in proj (parser.parse "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y")
