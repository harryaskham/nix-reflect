## nix-reflect
Pure-Nix parser, evaluator, and reflection utilities for Nix expressions.

### What is it?
`nix-reflect` provides:
- A parser that turns Nix source into a typed AST with source annotations
- An evaluator that can run on the AST (pure, monadic) or on text (via store-import)
- A small toolkit to build and transform text/AST functions (`fn`)
- Debug helpers for positional metadata and pretty-printing

Everything is written in Nix, no external binaries.

### Features
- **Parser**: Produces a rich AST for literals, `let`, `with`, `assert`, attribute sets (including `rec` and `inherit`), functions, application, operators (with precedence), paths (including `<nixpkgs>` form), strings (normal and indented), and more
- **Evaluator**:
  - AST mode: returns an `Either EvalError a` with a tracked evaluation state
  - Store mode: `import`-based evaluation of a string expression
  - Correct semantics for `with`, `assert`, logical short-circuit, attribute selection with `or`, recursive attrs, etc.
- **Functions (`fn`)**: Create callable functions from text or AST; transform the underlying text/AST
- **Debugging**: Position metadata and printers to inspect where values come from

### Add to your flake
```nix
# flake.nix
{
  inputs.nix-reflect.url = "github:harryaskham/collective-public?dir=flakes/nix-reflect";
  # Optional but recommended to avoid input duplication
  inputs.nix-reflect.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, nix-reflect, ... }@inputs:
    let
      system = builtins.currentSystem or "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      reflect = nix-reflect.lib; # System-specific lib
    in {
      # ... use `reflect` in your modules/derivations
    };
}
```

### Quick try in a REPL
- Load the flake:
```bash
nix repl 'github:harryaskham/collective-public?dir=flakes/nix-reflect'
```
- Then use `lib` directly:
```nix
lib.eval.eval.store "1 + 41"          # => 42
(lib.eval.eval.ast "1 + 1").right     # => 2 (Right value)
```

## Core modules
- **parser**: `lib.parser`
  - `parse :: (string | AST) -> AST`
  - `printAST :: AST -> string`
  - `read.fileFromAttrPath :: [string] -> path -> args -> string`
- **eval**: `lib.eval`
  - `eval.ast :: (string | AST) -> Either EvalError a`
  - `eval.store :: string -> a`
  - plus supporting modules: `eval.monad`, `eval.ast`, `eval.fn`, `eval.store`
- **fn**: `lib.eval.fn`
  - `fn.ast :: string -> { __fn :: a -> b; mapText; mapAST; }`
  - `fn.store :: string -> { __fn :: a -> b; mapText; }`
- **debuglib**: `lib.debuglib`
  - `pos`, `pathPos`, `printPos`, `printPosWith` for positional metadata

## Examples

### Parse and inspect an AST
```nix
let
  reflect = lib;  # from flake REPL: `lib`, or in your flake: `nix-reflect.lib`
  ast = reflect.parser.parse ''
    let a = 1; in a + 2
  '';
in reflect.parser.printAST ast
```
Example output (truncated):
```
  └─<λ x → _>
    ├─...
```

### Evaluate expressions
- AST mode (safe, returns Either):
```nix
with reflect.eval.monad;
let res = reflect.eval.eval.ast "if true then 1 else 2"; in
res.case { Left = e: toString e; Right = v: v; }  # => 1
```
- Store mode (convenient, uses `import`):
```nix
reflect.eval.eval.store "let a = 1; in a + 2"  # => 3
```

### Attribute selection and defaults
```nix
# Dot selection and `or` default
reflect.eval.eval.store ''{ a = 1; }.a''       # => 1
reflect.eval.eval.store ''{ a = 1; }.b or 42'' # => 42
```

### Lambdas as values with `fn`
```nix
let
  f = reflect.eval.fn.ast "x: x + 1";  # build a callable function from text
in f 41  # => 42
```
Transform the underlying text or AST and re-use:
```nix
let
  f = reflect.eval.fn.ast "a: b: 3 * a + b";
  g = f.mapText (t: "z: ${t} + z");
in g 5 3 1  # => 15
```

### Transform an AST and re-evaluate
```nix
let
  ast = reflect.parser.parse "1 + 2";
  transformed = ast.mapNode (n: n // { op = "-"; lhs = n.rhs; rhs = n.lhs; });
in (reflect.eval.eval.ast transformed).right  # => 1
```

### Read source by attribute path
Extract the source slice starting at an attribute path from a file:
```nix
reflect.parser.read.fileFromAttrPath [ "__testData" "deeper" "anExpr" ] \
  ./lib/parser/default.nix { inherit lib (lib) collective-lib nix-parsec; }
```
Returns a `string` fragment beginning at the requested attr’s position.

### Positional metadata and printing
```nix
let
  pos = reflect.debuglib.pos "abc" { abc = 123; def = 456; };
in {
  fileOnly = (reflect.debuglib.pos.file "abc" { abc = 123; }).file;
  pretty = reflect.debuglib.printPos pos;
}
```

## Notes and limitations
- The parser covers a wide range of Nix syntax and is exercised with property and smoke tests inside the repo
- The AST evaluator models Nix semantics for control-flow, attrs, and operators (including short-circuiting), with typed errors (`EvalError` variants)
- Angle paths like `<nixpkgs>` resolve via `NIX_PATH` in the evaluation scope
- Some advanced forms (e.g., dynamic derivations in nested contexts) are noted as TODOs; see inline comments in `lib/eval` and `lib/parser`

## Development
- `lib` is exposed under the flake; load it in a REPL or import the module in your own flake
- A lightweight dev shell is available:
```bash
nix develop 'github:harryaskham/collective-public?dir=flakes/nix-reflect'
```
- Tests live alongside modules under `lib/...` as `_tests` attributes; evaluate them via your preferred harness or REPL exploration
