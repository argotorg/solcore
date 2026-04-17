#!/usr/bin/env python3
"""
bnf2railroad.py — BNF/EBNF grammar file -> SVG railroad diagrams

Usage
  python3 bnf2railroad.py GRAMMAR.bnf OUTPUT_DIR/
"""

import io
import os
import re
import sys

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
    """Write diagram to path, injecting xmlns and embedded CSS."""
    buf = io.StringIO()
    diagram.writeSvg(buf.write)
    svg = buf.getvalue()
    # Add XML namespace so browsers render the file when loaded via <img>
    svg = svg.replace('<svg ', '<svg xmlns="http://www.w3.org/2000/svg" ', 1)
    # Embed CSS so the SVG is self-contained (no external stylesheet needed)
    svg = svg.replace('<g transform=', _STYLE + '\n<g transform=', 1)
    with open(path, 'w') as fh:
        fh.write(svg)


def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    if len(argv) < 2:
        print(__doc__)
        sys.exit(1)

    grammar_file, output_dir = argv[0], argv[1]
    os.makedirs(output_dir, exist_ok=True)

    rules = read_rules(grammar_file)
    print(f"Loaded {len(rules)} rule(s) from {grammar_file!r}")

    ok = failed = 0
    for name, body in rules.items():
        if not body.strip():
            print(f"  skip   {name}  (empty body)")
            continue
        try:
            ast = _parse_body(body)
            diagram = rr.Diagram(_to_rr(ast))
            out = os.path.join(output_dir, f"{name}.svg")
            _write_svg(diagram, out)
            print(f"  wrote  {out}")
            ok += 1
        except Exception as exc:
            print(f"  ERROR  {name}: {exc}")
            failed += 1

    print(f"\n{ok} diagram(s) written, {failed} error(s).")
    if failed:
        sys.exit(1)


if __name__ == '__main__':
    main()
