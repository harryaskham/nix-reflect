"""Figures and generated regions for the nix-reflect site.

Everything here is drawn from data/*.json, which capture.py records by running the library."""
import html
import json
import re
from pathlib import Path

from diagrams import graph, text_w

DATA = Path(__file__).resolve().parent / "data"
METRICS = {"sans": 1.0, "mono": 0.6}


def load(name):
    return json.loads((DATA / name).read_text())


# ── the parse tree ──────────────────────────────────────────────────────────

KIND = {"letIn": "accent", "assignment": "b", "attrs": "b", "binaryOp": "c", "identifier": "ink",
        "int": "a", "attrPath": "muted"}


def _children(n):
    out = []
    for key, v in n["kids"].items():
        if "label" in v:
            out.append((key, v))
        elif "list" in v:
            out += [(f"{key}[{i}]", x) for i, x in enumerate(v["list"]) if "label" in x]
    return out


def _clip(s, width, size=12.5, keep_spaces=False):
    """Shorten s to fit width; whitespace is collapsed unless keep_spaces (values whose spacing matters)."""
    s = s.replace("\n", " ") if keep_spaces else " ".join(s.split())
    if text_w(s, size) <= width:
        return s
    while s and text_w(s + "…", size) > width:
        s = s[:-1]
    return s.rstrip() + "…"


def tree():
    root = load("ast.json")
    nodes, edges = [], []
    gap, row, top, margin = 12, 92, 40, 24

    def base_w(n):
        if _children(n):
            return max(110, text_w(n["label"], 15, True) + 32)
        return max(54, text_w(n["label"], 15, True) + 26)

    span = {}

    def measure(n):
        kids = _children(n)
        total = sum(measure(k) for _, k in kids) + gap * max(0, len(kids) - 1)
        span[id(n)] = max(base_w(n), total)
        return span[id(n)]

    measure(root)
    counter = [0]

    def emit(n, left, depth, key=None, parent=None):
        counter[0] += 1
        nid = f"n{counter[0]}"
        kids = _children(n)
        s_w = span[id(n)]
        if kids:
            total = sum(span[id(k)] for _, k in kids) + gap * (len(kids) - 1)
            cursor = left + (s_w - total) / 2
            centres = []
            child_ids = []
            for _, k in kids:
                centres.append(cursor + span[id(k)] / 2)
                cursor += span[id(k)] + gap
            x = (centres[0] + centres[-1]) / 2
            room = 2 * min(x - left, left + s_w - x)
            sub = _clip(n["src"] or "", max(60.0, room - 28))
            w = min(room, max(base_w(n), text_w(sub, 12.5) + 28))
            node = {"id": nid, "x": x, "y": top + depth * row, "label": n["label"], "mono": True,
                    "kind": KIND.get(n["type"], "ink"), "sub": sub, "w": w}
        else:
            x = left + s_w / 2
            node = {"id": nid, "x": x, "y": top + depth * row, "label": n["label"], "mono": True,
                    "kind": KIND.get(n["type"], "ink"), "w": base_w(n)}
        nodes.append(node)
        if parent:
            edges.append({"from": parent, "to": nid, "kind": "muted", "out": "b", "in": "t",
                          "mid": top + depth * row - row / 2 + 4, "label": key, "mono": True, "at": 0.78, "r": 8})
        if kids:
            cursor = left + (s_w - total) / 2
            for k_key, k in kids:
                emit(k, cursor, depth + 1, k_key, nid)
                cursor += span[id(k)] + gap
        return depth

    emit(root, margin, 0)
    depth = max(round((nd["y"] - top) / row) for nd in nodes)
    width = span[id(root)] + 2 * margin
    height = top + depth * row + 44
    return graph(
        "tree", "The parse tree of one expression",
        "The tree that parser.parse returns for: let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y. "
        "The root is a let-in node whose binding assigns a recursive attribute set to a, with x bound to 1 + 1 and "
        "y to x + 1; its body adds a.x and a.y, each an attribute selection. Every node is labelled the way the "
        "library prints it and, where it has children, carries the exact slice of source text it was parsed from. "
        "Edges are labelled with the field that holds the child, such as lhs, rhs, body or bindings[0].",
        width, height, nodes, edges)


# ── the evaluator ───────────────────────────────────────────────────────────

def evaluator():
    return graph(
        "evaluator", "From text to a value",
        "How eval works. Source text is parsed by parser combinators into an AST of attribute sets. evalM walks the "
        "AST, dispatching on each node's type, inside an evaluation monad: a computation that threads an EvalState "
        "(the scope of names in reach, a cache of thunks and their forced values, and a stack used for traces) and "
        "can stop with a typed EvalError. eval.either returns Right value or Left error; eval returns the value or "
        "throws the error.",
        950, 330,
        nodes=[
            dict(id="src", x=90, y=70, label="source text", sub="\"1 + 2\"", kind="file", w=140),
            dict(id="parse", x=280, y=70, label="parser.parse", sub="nix-parsec combinators", kind="ink", mono=True, w=170),
            dict(id="ast", x=470, y=70, label="AST", sub="nodeType · fields · __src", kind="b", w=170),
            dict(id="evalm", x=665, y=70, label="evalM", sub="dispatch on nodeType", kind="accent", mono=True, w=160),
            dict(id="val", x=862, y=70, label="Right 3", sub="eval returns 3", kind="ok", mono=True, w=140),
            dict(id="err", x=420, y=236, label="Left EvalError", sub="Throw · TypeError · …", kind="err", mono=True, w=180),
            dict(id="scope", x=620, y=236, label="scope", sub="names in reach", kind="muted", w=118),
            dict(id="cache", x=752, y=236, label="thunk cache", sub="forced values", kind="muted", w=126),
            dict(id="stack", x=876, y=236, label="stack", sub="for traces", kind="muted", w=100),
        ],
        edges=[
            {"from": "src", "to": "parse", "kind": "ink"},
            {"from": "parse", "to": "ast", "kind": "ink"},
            {"from": "ast", "to": "evalm", "kind": "ink"},
            {"from": "evalm", "to": "val", "kind": "ok"},
            {"from": "evalm", "to": "err", "kind": "err", "out": "b", "in": "t", "ox": -24, "mid": 150, "label": "or stops", "at": 0.55},
            {"from": "evalm", "to": "cache", "kind": "muted", "out": "b", "in": "t", "ox": 24, "mid": 150, "style": "dash", "both": True,
             "label": "threads", "at": 0.5},
        ],
        groups=[{"nodes": ["scope", "cache", "stack"], "title": "EvalState", "kind": "muted", "pad": 16, "dashed": True}],
    )


FIGS = {"tree": tree, "evaluator": evaluator}


# ── generated regions ───────────────────────────────────────────────────────

def _pre(text, cls=""):
    return f'<pre tabindex="0"><code{cls}>{html.escape(text.rstrip())}</code></pre>'


def _result(text):
    """A result block: the output exactly, minus trailing spaces and blank lines."""
    lines = [l.rstrip() for l in text.splitlines() if l.strip()]
    return f'<pre class="result" tabindex="0" aria-label="Result"><code>{html.escape(chr(10).join(lines))}</code></pre>'


def _out(key):
    return lambda: _result(load("outputs.json")[key])


def _norm(s):
    return re.sub(r"\s+", " ", s).strip()


def _shown(expr):
    """Expressions as typed; a multi-line one keeps its line breaks (the cell wraps on them)."""
    return expr if "\n" in expr else " ".join(expr.split())


def _verdict(r):
    n, x = r["nix"], r["reflect"]
    if not n["ok"] and not x["ok"]:
        return "same", "both refuse"
    if n["ok"] and x["ok"] and _norm(n["out"]) == _norm(x["out"]):
        return "same", "same"
    if not r.get("parses", True):
        return "diff", "does not parse"
    if not x["ok"]:
        e = x["err"]
        if "stack overflow" in e:
            return "diff", "stack overflow"
        m = re.search(r"EvalError\.(\w+)", e)
        if m and m.group(1) == "SyntaxError":
            return "diff", "fails while evaluating"
        if m:
            return "diff", m.group(1)
        return "diff", "internal error"
    return "diff", "different value"


def conformance():
    rows, group = [], None
    for r in load("probe.json"):
        if r["group"] != group:
            group = r["group"]
            rows.append(f'<tr class="grp"><th colspan="4" scope="rowgroup">{html.escape(group)}</th></tr>')
        verdict, why = _verdict(r)
        nix = r["nix"]["out"] if r["nix"]["ok"] else "error"
        refl = r["reflect"]["out"] if r["reflect"]["ok"] else why
        if verdict == "diff" and r["reflect"]["ok"]:
            refl = r["reflect"]["out"]
        t = f'{r["reflect"]["s"]:.1f} s'
        mode = '<span class="mode">lazy</span> ' if r["mode"] == "lazy" else ""
        rows.append(
            f'<tr class="{verdict}"><td data-label="Expression"><code>{html.escape(_shown(r["expr"]))}</code></td>'
            f'<td data-label="Nix"><code>{html.escape(_clip(nix, 150, 13, keep_spaces=True))}</code></td>'
            f'<td class="v" data-label="nix-reflect"><span class="mark" aria-label="{"matches" if verdict == "same" else "differs"}">{"✓" if verdict == "same" else "✗"}</span> '
            f'{"<code>" + html.escape(_clip(refl, 170, 13, keep_spaces=True)) + "</code>" if r["reflect"]["ok"] else html.escape(refl)}</td>'
            f'<td class="t" data-label="Time">{mode}{t}</td></tr>')
    return ('<table class="conf">\n<colgroup><col class="c-expr"><col class="c-nix"><col class="c-refl"><col class="c-t"></colgroup>\n<caption>The same expressions, evaluated by Nix and by nix-reflect</caption>\n'
            '<thead><tr><th scope="col">Expression</th><th scope="col">Nix</th><th scope="col">nix-reflect</th>'
            '<th scope="col">Time</th></tr></thead>\n<tbody>\n' + "\n".join(rows) + "\n</tbody>\n</table>")


def summary():
    rs = load("probe.json")
    same = sum(_verdict(r)[0] == "same" for r in rs)
    noparse = sum(_verdict(r)[1] == "does not parse" for r in rs)
    return (f'<p class="sum"><b>{same} of {len(rs)}</b> agree with Nix. Of the rest, {noparse} do not parse, and '
            f'{len(rs) - same - noparse} parse but evaluate differently.</p>')


GEN = {
    "conformance": conformance,
    "summary": summary,
    "out-value": _out("value"),
    "out-rewrite": _out("rewrite"),
    "out-unknown": _out("unknown"),
    "out-type": _out("typeError"),
    "out-missing": _out("missing"),
    "out-store": _out("store"),
    "out-src": lambda: _result(json.dumps(load("outputs.json")["src"])),
}
