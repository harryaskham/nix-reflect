# Example outputs quoted on the site. Each value is what the library returns, as a string.
lib: with lib; {
  value = toString (eval "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y");
  rewrite = toString (
    let
      ast = parser.parse "1 + 2";
      swapped = ast.mapNode (n: n // { op = "-"; lhs = n.rhs; rhs = n.lhs; });
    in eval swapped);
  rightTwo = toString (eval.either "1 + 1");
  unknown = toString (eval.either "let x = 1; in y");
  missing = toString (eval.either "{ a = 1; }.b");
  typeError = toString (eval.either "1 + \"a\"");
  store = toString (eval.eval.store "1 + 1");
  nodeType = (parser.parse "1 + 2").nodeType;
  src = (parser.parse "  1   +   2").__src;
}
