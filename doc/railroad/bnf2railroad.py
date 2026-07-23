#!/usr/bin/env python3
"""
bnf2railroad.py — BNF/EBNF grammar file -> SVG railroad diagrams

Usage
  python3 bnf2railroad.py [--clean | --check] GRAMMAR.bnf OUTPUT_DIR/
"""

import filecmp
import io
import os
import re
import sys
import tempfile

try:
    import railroad as rr
except ImportError:
    sys.exit(
        "railroad-diagrams is required.\n"
        "Install inside a nix-shell with:\n"
        "  nix-shell -p python3Packages.railroad-diagrams"
    )


_STYLE = """\
<style>
svg.railroad-diagram { background: #fafafa; }
svg.railroad-diagram path { stroke-width: 2; stroke: #333; fill: none; }
svg.railroad-diagram text {
    font: bold 14px monospace; fill: #333;
    text-anchor: middle;
}
svg.railroad-diagram text.comment { font: italic 12px monospace; fill: #888; }
svg.railroad-diagram rect { stroke-width: 2; stroke: #333; fill: #fff; }
svg.railroad-diagram rect.group-box {
    stroke: #aaa; stroke-dasharray: 4 2; fill: none;
}
</style>"""

_OUTPUT_MARKER = ".bnf2railroad-output"
_OUTPUT_MARKER_TEXT = """\
This directory is owned by doc/railroad/bnf2railroad.py.
All SVG files in it may be removed by --clean.
"""


def _strip_line_comment(line: str) -> str:
    """Remove a trailing # or -- comment, respecting quoted strings."""
    i = 0
    while i < len(line):
        c = line[i]
        if c in ('"', "'"):
            i += 1
            while i < len(line) and line[i] != c:
                i += 1
            i += 1
        elif c == '#' or line[i : i + 2] == '--':
            return line[:i]
        else:
            i += 1
    return line


def read_rules(path: str) -> dict:
    """Parse a BNF file and return {rule_name: body_string} in file order."""
    with open(path) as fh:
        text = fh.read()

    rules: dict = {}
    order: list = []
    current_name: str | None = None
    current_parts: list = []

    def _flush():
        if current_name is not None:
            rules[current_name] = " ".join(current_parts)

    for raw_line in text.splitlines():
        line = _strip_line_comment(raw_line)
        stripped = line.strip()
        if not stripped:
            continue

        # A rule-definition line starts at column 0 with an identifier and = or ::=
        m = re.match(r'^([A-Za-z_][A-Za-z0-9_]*)\s*(?:=|::=)(.*)', line)
        if m and not raw_line[0].isspace():
            _flush()
            current_name = m.group(1)
            current_parts = [m.group(2).strip()]
            if current_name not in rules:
                order.append(current_name)
        elif current_name is not None and stripped:
            # Continuation line (indented or starts with |)
            current_parts.append(stripped)

    _flush()
    return {k: rules[k] for k in order}


_OPERATORS = {
    '|': 'PIPE',
    '[': 'LBRACK', ']': 'RBRACK',
    '{': 'LBRACE', '}': 'RBRACE',
    '(': 'LPAREN', ')': 'RPAREN',
    '+': 'PLUS',
    '*': 'STAR',
    '?': 'QUEST',
}


def _tokenize(text: str) -> list:
    """Return a list of (kind, value) pairs; terminated by ('EOF', '')."""
    tokens = []
    pos = 0
    while pos < len(text):
        c = text[pos]
        if c.isspace():
            pos += 1
            continue
        if c in ('"', "'"):
            try:
                end = text.index(c, pos + 1)
            except ValueError:
                raise SyntaxError(f"Unterminated string starting at position {pos}")
            tokens.append(('QUOTED', text[pos + 1 : end]))
            pos = end + 1
        elif c.isalpha() or c == '_':
            m = re.match(r'[A-Za-z_][A-Za-z0-9_]*', text[pos:])
            tokens.append(('NAME', m.group()))
            pos += len(m.group())
        elif c in _OPERATORS:
            tokens.append((_OPERATORS[c], c))
            pos += 1
        else:
            raise SyntaxError(
                f"Unexpected character {c!r} near: {text[pos : pos + 20]!r}"
            )
    tokens.append(('EOF', ''))
    return tokens


class _Parser:
    _STOP = frozenset({'PIPE', 'RBRACK', 'RBRACE', 'RPAREN', 'EOF'})

    def __init__(self, tokens: list):
        self._tokens = tokens
        self._pos = 0

    def _peek(self):
        return self._tokens[self._pos]

    def _consume(self, expected_kind: str | None = None):
        tok = self._tokens[self._pos]
        if expected_kind and tok[0] != expected_kind:
            raise SyntaxError(f"Expected {expected_kind!r}, got {tok!r}")
        self._pos += 1
        return tok

    # -- public entry point

    def parse(self):
        node = self._alternatives()
        self._consume('EOF')
        return node

    # -- recursive descent

    def _alternatives(self):
        alts = [self._sequence()]
        while self._peek()[0] == 'PIPE':
            self._consume('PIPE')
            alts.append(self._sequence())
        return alts[0] if len(alts) == 1 else ('choice', alts)

    def _sequence(self):
        items = []
        while self._peek()[0] not in self._STOP:
            items.append(self._item())
        if not items:
            return ('empty',)
        return items[0] if len(items) == 1 else ('seq', items)

    def _item(self):
        atom = self._atom()
        kind = self._peek()[0]
        if kind == 'PLUS':
            self._consume('PLUS')
            return ('rep1', atom)
        if kind == 'STAR':
            self._consume('STAR')
            return ('rep0', atom)
        if kind == 'QUEST':
            self._consume('QUEST')
            return ('opt', atom)
        return atom

    def _atom(self):
        kind, val = self._peek()
        if kind == 'QUOTED':
            self._consume()
            return ('term', val)
        if kind == 'NAME':
            self._consume()
            return ('nt', val)
        if kind == 'LBRACK':
            self._consume('LBRACK')
            inner = self._alternatives()
            self._consume('RBRACK')
            return ('opt', inner)
        if kind == 'LBRACE':
            self._consume('LBRACE')
            inner = self._alternatives()
            self._consume('RBRACE')
            return ('rep0', inner)
        if kind == 'LPAREN':
            self._consume('LPAREN')
            inner = self._alternatives()
            self._consume('RPAREN')
            return inner
        raise SyntaxError(f"Unexpected token {self._peek()!r} while parsing atom")


def _parse_body(body: str):
    return _Parser(_tokenize(body)).parse()


def _to_rr(node):
    """Recursively convert a grammar AST node to a railroad-diagrams element."""
    kind = node[0]
    if kind == 'term':
        return rr.Terminal(node[1])
    if kind == 'nt':
        return rr.NonTerminal(node[1])
    if kind == 'seq':
        parts = [_to_rr(x) for x in node[1]]
        return rr.Sequence(*parts) if len(parts) > 1 else parts[0]
    if kind == 'choice':
        alts = [_to_rr(x) for x in node[1]]
        return rr.Choice(0, *alts) if len(alts) > 1 else alts[0]
    if kind == 'opt':
        return rr.Optional(_to_rr(node[1]))
    if kind == 'rep0':
        return rr.ZeroOrMore(_to_rr(node[1]))
    if kind == 'rep1':
        return rr.OneOrMore(_to_rr(node[1]))
    if kind == 'empty':
        return rr.Terminal('\u03b5')   # ε
    raise ValueError(f"Unknown AST node kind {kind!r}")


def _write_svg(diagram, path: str):
    """Atomically write a diagram without following an output symlink."""
    buf = io.StringIO()
    diagram.writeSvg(buf.write)
    svg = buf.getvalue()
    # Add XML namespace so browsers render the file when loaded via <img>
    svg = svg.replace('<svg ', '<svg xmlns="http://www.w3.org/2000/svg" ', 1)
    # Embed CSS so the SVG is self-contained (no external stylesheet needed)
    svg = svg.replace('<g transform=', _STYLE + '\n<g transform=', 1)
    if os.path.islink(path):
        sys.exit(f"refusing to replace symlink output {path!r}")

    temporary_path = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            dir=os.path.dirname(path),
            prefix=f".{os.path.basename(path)}.",
            suffix=".tmp",
            delete=False,
        ) as temporary:
            temporary_path = temporary.name
            temporary.write(svg)
        os.replace(temporary_path, path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            try:
                os.unlink(temporary_path)
            except FileNotFoundError:
                pass


def _marker_path(output_dir: str) -> str:
    return os.path.join(output_dir, _OUTPUT_MARKER)


def mark_generated_output_dir(output_dir: str) -> None:
    marker_path = _marker_path(output_dir)
    try:
        with open(marker_path, "x") as marker:
            marker.write(_OUTPUT_MARKER_TEXT)
    except FileExistsError:
        sys.exit(
            f"refusing to replace existing output marker {marker_path!r}"
        )


def output_dir_is_owned(output_dir: str) -> bool:
    marker_path = _marker_path(output_dir)
    if not os.path.lexists(marker_path):
        return False
    try:
        if os.path.islink(marker_path):
            raise OSError("marker must not be a symlink")
        with open(marker_path) as marker:
            marker_text = marker.read()
    except (FileNotFoundError, OSError) as exc:
        sys.exit(
            f"refusing output directory {output_dir!r}: "
            f"{exc}"
        )
    if marker_text != _OUTPUT_MARKER_TEXT:
        sys.exit(
            f"refusing output directory {output_dir!r}: "
            f"invalid {_OUTPUT_MARKER} marker"
        )
    return True


def require_owned_output_dir(output_dir: str, operation: str) -> None:
    if not output_dir_is_owned(output_dir):
        sys.exit(
            f"refusing {operation} for unowned output directory {output_dir!r}: "
            f"missing {_OUTPUT_MARKER} marker"
        )


def prepare_generated_output_dir(output_dir: str) -> None:
    if not os.path.lexists(output_dir):
        os.makedirs(output_dir)
    if os.path.islink(output_dir) or not os.path.isdir(output_dir):
        sys.exit(
            f"refusing unsafe output directory {output_dir!r}: "
            "expected a real directory, not a symlink or file"
        )
    if output_dir_is_owned(output_dir):
        return
    entries = os.listdir(output_dir)
    if entries:
        sys.exit(
            f"refusing to generate into unowned non-empty output directory "
            f"{output_dir!r}; initialize a new or empty directory instead"
        )
    mark_generated_output_dir(output_dir)


def reject_svg_symlinks(output_dir: str, operation: str) -> None:
    for entry in os.scandir(output_dir):
        if entry.name.endswith(".svg") and entry.is_symlink():
            sys.exit(
                f"refusing {operation} with symlink output "
                f"{entry.path!r}"
            )


def generate(grammar_file: str, output_dir: str) -> set[str]:
    prepare_generated_output_dir(output_dir)
    reject_svg_symlinks(output_dir, "generation")

    rules = read_rules(grammar_file)
    print(f"Loaded {len(rules)} rule(s) from {grammar_file!r}")

    ok = failed = 0
    generated: set[str] = set()
    for name, body in rules.items():
        if not body.strip():
            print(f"  skip   {name}  (empty body)")
            continue
        try:
            ast = _parse_body(body)
            diagram = rr.Diagram(_to_rr(ast))
            out = os.path.join(output_dir, f"{name}.svg")
            _write_svg(diagram, out)
            generated.add(f"{name}.svg")
            print(f"  wrote  {out}")
            ok += 1
        except Exception as exc:
            print(f"  ERROR  {name}: {exc}")
            failed += 1

    print(f"\n{ok} diagram(s) written, {failed} error(s).")
    if failed:
        sys.exit(1)
    return generated


def svg_names(directory: str) -> set[str]:
    return {
        entry.name
        for entry in os.scandir(directory)
        if entry.name.endswith(".svg") and entry.is_file(follow_symlinks=False)
    }


def check_generated(grammar_file: str, output_dir: str) -> None:
    require_owned_output_dir(output_dir, "--check")
    reject_svg_symlinks(output_dir, "--check")
    with tempfile.TemporaryDirectory(prefix="sail-railroad-") as expected_dir:
        expected = generate(grammar_file, expected_dir)
        actual = svg_names(output_dir)
        missing = sorted(expected - actual)
        stale = sorted(actual - expected)
        changed = sorted(
            name
            for name in expected & actual
            if not filecmp.cmp(
                os.path.join(expected_dir, name),
                os.path.join(output_dir, name),
                shallow=False,
            )
        )
    if missing or stale or changed:
        for label, names in (
            ("missing", missing),
            ("stale", stale),
            ("out of date", changed),
        ):
            for name in names:
                print(f"  {label}: {name}", file=sys.stderr)
        sys.exit("railroad diagrams are not synchronized with the grammar")
    print(f"All {len(expected)} railroad diagram(s) are up to date.")


def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    check = "--check" in argv
    clean = "--clean" in argv
    positional = [arg for arg in argv if arg not in {"--check", "--clean"}]
    if check and clean:
        sys.exit("--check and --clean cannot be used together")
    if len(positional) != 2:
        print(__doc__)
        sys.exit(1)

    grammar_file, output_dir = positional
    if check:
        check_generated(grammar_file, output_dir)
        return

    if clean:
        require_owned_output_dir(output_dir, "--clean")
    generated = generate(grammar_file, output_dir)
    if clean:
        stale = sorted(svg_names(output_dir) - generated)
        for name in stale:
            os.remove(os.path.join(output_dir, name))
            print(f"  removed stale  {os.path.join(output_dir, name)}")


if __name__ == '__main__':
    main()
