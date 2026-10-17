#!/usr/bin/env python3
"""Record the real outputs the site quotes, by running the library itself.

    python3 scripts/site/capture.py            # all of it (a few minutes; needs nix)
    python3 scripts/site/capture.py probe      # only the conformance probe

Writes scripts/site/data/{ast.json, outputs.json, probe.json}, then run build.py.

Everything runs as `nix eval` against this flake's `lib` output in a clean environment
(env -i, a scratch HOME), so no personal configuration or credentials can leak in.

The probe evaluates each expression twice: with the real Nix evaluator (`nix eval --file`) and
with nix-reflect. Expressions go through a file, never a shell, so quoting can't alter them.
nix-reflect is called as `eval.strict` so data results are fully forced (as `nix eval` prints
them); the laziness and recursion cases use the default lazy `eval`.
"""
import json
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
DATA = HERE / "data"
LAZY = {"laziness", "recursion"}

CASES = [
    ("arithmetic", "1 + 2 * 3"),
    ("arithmetic", "7 / 2"),
    ("arithmetic", "1.5 * 2"),
    ("strings", 'let name = "nix"; in "hello ${name}"'),
    ("strings", "''\n  indented\n    block\n''"),
    ("strings", '"a" + "b" + toString 3'),
    ("lists", "[ 1 2 ] ++ [ 3 ]"),
    ("lists", "builtins.map (x: x * 2) [ 1 2 3 ]"),
    ("lists", "builtins.foldl' (a: b: a + b) 0 [ 1 2 3 4 ]"),
    ("attrsets", "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y"),
    ("attrsets", "{ a = 1; } // { b = 2; }"),
    ("attrsets", "let x = 1; in { inherit x; y = 2; }"),
    ("attrsets", "{ a.b.c = 1; }"),
    ("attrsets", "{ a = 1; }.b or 42"),
    ("attrsets", "builtins.attrNames { b = 1; a = 2; }"),
    ("functions", "({ a, b ? 10, ... }: a + b) { a = 1; c = 3; }"),
    ("functions", "(x: y: x - y) 10 3"),
    ("functions", "let f = args@{ a, ... }: args.b; in f { a = 1; b = 2; }"),
    ("control", 'if 1 < 2 then "yes" else "no"'),
    ("control", "let x = { y = 1; }; in with x; y + 1"),
    ("control", 'assert 1 == 1; "ok"'),
    ("operators", "{ a = 1; } ? a"),
    ("operators", "true -> false"),
    ("operators", "!(1 == 2) && true"),
    ("laziness", 'let xs = [ 1 (throw "boom") 3 ]; in builtins.elemAt xs 2'),
    ("laziness", '{ a = 1; b = throw "boom"; }.a'),
    ("laziness", 'let x = throw "boom"; in 1'),
    ("recursion", "let fib = n: if n < 2 then n else fib (n - 1) + fib (n - 2); in fib 5"),
    ("recursion", "let fib = n: if n < 2 then n else fib (n - 1) + fib (n - 2); in fib 7"),
    ("recursion", "let fib = n: if n < 2 then n else fib (n - 1) + fib (n - 2); in fib 10"),
    ("errors", "let x = 1; in y"),
    ("errors", '1 + "a"'),
]


def env(home):
    path = os.environ.get("PATH", "/usr/bin:/bin")
    base = {"PATH": path, "HOME": home, "CLTV_TRACE_LEVEL": "0",
            "NIX_CONFIG": "experimental-features = nix-command flakes"}
    nixpkgs = subprocess.run(["nix", "eval", "--raw", ".#inputs.nixpkgs.outPath"], cwd=ROOT, env=base,
                             capture_output=True, text=True).stdout.strip()
    return {**base, "NIX_PATH": f"nixpkgs={nixpkgs}"}  # for the tests that import <nixpkgs/lib>


def run(args, e, timeout):
    t = time.time()
    try:
        p = subprocess.run(args, cwd=ROOT, env=e, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return {"ok": False, "out": "", "err": f"timeout after {timeout}s", "s": timeout}
    err = "" if p.returncode == 0 else last_error(p.stderr)
    return {"ok": p.returncode == 0, "out": p.stdout.strip() if p.returncode == 0 else "", "err": err,
            "s": round(time.time() - t, 1)}


def last_error(stderr):
    lines = [l.rstrip() for l in stderr.splitlines() if l.strip() and not l.startswith("warning:")]
    for i in range(len(lines) - 1, -1, -1):
        if lines[i].lstrip().startswith("error:"):
            return " ".join(l.strip() for l in lines[i:i + 3]).replace("error: ", "", 1)[:220]
    return (lines[-1] if lines else "")[:220]


def apply(expr_file, e, json_out=True, timeout=600):
    args = ["nix", "eval", "--option", "max-call-depth", "1000000"] + (["--json"] if json_out else []) + \
           ["--apply", Path(expr_file).read_text(), ".#lib.x86_64-linux"]
    r = run(args, e, timeout)
    if not r["ok"]:
        sys.exit(f"capture failed for {expr_file}: {r['err']}")
    return json.loads(r["out"])


def probe(e, tmp):
    results = []
    for group, expr in CASES:
        src = os.path.join(tmp, "e.nix")
        Path(src).write_text(expr)
        real = run(["nix", "eval", "--impure", "--file", src], e, 60)
        how = "" if group in LAZY else ".strict"
        refl = run(["nix", "eval", "--impure", "--option", "max-call-depth", "1000000", "--apply",
                    f"lib: lib.eval{how} (builtins.readFile {src})", ".#lib.x86_64-linux"], e, 180)
        parse = run(["nix", "eval", "--impure", "--json", "--apply",
                     f"lib: (builtins.tryEval (lib.parser.parse (builtins.readFile {src})).nodeType).success",
                     ".#lib.x86_64-linux"], e, 120)
        results.append({"group": group, "expr": expr, "mode": "lazy" if group in LAZY else "strict",
                        "parses": parse["out"] == "true", "nix": real, "reflect": refl})
        print(f"{group:10s} {expr[:44]!r:48s} nix={real['out'][:16] or 'error':16s} "
              f"nix-reflect={refl['out'][:16] or 'error'} ({refl['s']}s)", flush=True)
    return results


def main():
    which = set(sys.argv[1:]) or {"ast", "outputs", "probe"}
    DATA.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        e = env(os.path.join(tmp, "home"))
        if "ast" in which:
            (DATA / "ast.json").write_text(json.dumps(apply(HERE / "nix/ast.nix", e), indent=1, ensure_ascii=False) + "\n")
        if "outputs" in which:
            (DATA / "outputs.json").write_text(json.dumps(apply(HERE / "nix/outputs.nix", e), indent=1, ensure_ascii=False) + "\n")
        if "probe" in which:
            (DATA / "probe.json").write_text(json.dumps(probe(e, tmp), indent=1, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
