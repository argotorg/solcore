#!/usr/bin/env python3
"""Migrate the previous Solcore syntax to solcore-rs/new-syntax.

Comments, literals and assembly blocks are preserved. Only the tracked Core
source corpus can be written; symlink sources are never followed. Expression
annotations become typed local bindings so their expected type remains explicit
after removal of `as` expressions. Lazy operands and earlier effectful argument
prefixes retain identity applications to preserve their evaluation order.
"""

from __future__ import annotations

import argparse
import dataclasses
import os
import pathlib
import re
import subprocess
import sys
from collections.abc import Iterable, Sequence


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]

CORE_SOL_FILES = (
    "blog-post/adjust.sol",
    "blog-post/erc20.sol",
    "blog-post/payment.sol",
    "blog-post/sum.sol",
    "concept-art/has-field.sol",
)

PACKAGED_SOLC_ROOTS = ("src", "std", "test")

CLASSIC_SOL_FILES = frozenset(
    {
        "blog-post/PaymentHandler.sol",
        "lib/StdAssertions.sol",
        "lib/Vm.sol",
        "lib/console.sol",
        "lib/stdlib.sol",
        "test/examples/dispatch/fib.classic.sol",
    }
)

SPECIAL_FIXTURES = {}
TRIVIA_KINDS = frozenset({"space", "comment"})

@dataclasses.dataclass(frozen=True)
class Token:
    kind: str
    text: str
    start: int
    end: int


@dataclasses.dataclass(frozen=True)
class Edit:
    start: int
    end: int
    replacement: str


def tokenize(source: str) -> list[Token]:
    """Lex enough of Solcore to distinguish code from comments and strings."""

    tokens: list[Token] = []
    i = 0
    n = len(source)
    multi = (
        "->",
        "=>",
        ":=",
        "==",
        "!=",
        "<=",
        ">=",
        "&&",
        "||",
        "+=",
        "-=",
        "^=",
        "&=",
        "|=",
        "%=",
        "**",
    )

    while i < n:
        start = i
        ch = source[i]

        if ch.isspace():
            i += 1
            while i < n and source[i].isspace():
                i += 1
            tokens.append(Token("space", source[start:i], start, i))
            continue

        if source.startswith("//", i):
            newline = source.find("\n", i + 2)
            i = n if newline < 0 else newline
            tokens.append(Token("comment", source[start:i], start, i))
            continue

        if source.startswith("/*", i):
            depth = 1
            i += 2
            while i < n and depth > 0:
                if source.startswith("/*", i):
                    depth += 1
                    i += 2
                elif source.startswith("*/", i):
                    depth -= 1
                    i += 2
                else:
                    i += 1
            tokens.append(Token("comment", source[start:i], start, i))
            continue

        if ch in {'"', "'"}:
            quote = ch
            i += 1
            while i < n:
                if source[i] == "\\":
                    i = min(i + 2, n)
                elif source[i] == quote:
                    i += 1
                    break
                else:
                    i += 1
            tokens.append(Token("string", source[start:i], start, i))
            continue

        if ch == "`":
            close = source.find("`", i + 1)
            i = n if close < 0 else close + 1
            tokens.append(Token("meta", source[start:i], start, i))
            continue

        if source.startswith("${", i):
            close = source.find("}", i + 2)
            i = n if close < 0 else close + 1
            tokens.append(Token("meta", source[start:i], start, i))
            continue

        if ch.isalpha() or ch == "_":
            i += 1
            while i < n and (source[i].isalnum() or source[i] == "_"):
                i += 1
            tokens.append(Token("ident", source[start:i], start, i))
            continue

        if ch.isdigit():
            i += 1
            while i < n and (
                source[i].isalnum() or source[i] in {"_", "."}
            ):
                i += 1
            tokens.append(Token("number", source[start:i], start, i))
            continue

        op = next((candidate for candidate in multi if source.startswith(candidate, i)), None)
        if op is not None:
            i += len(op)
            tokens.append(Token("symbol", op, start, i))
            continue

        i += 1
        tokens.append(Token("symbol", ch, start, i))

    return tokens


def significant(source: str) -> list[Token]:
    return [token for token in tokenize(source) if token.kind not in TRIVIA_KINDS]


def apply_edits(source: str, edits: Iterable[Edit]) -> str:
    """Apply non-overlapping edits, coalescing insertions at one position."""

    ordered = sorted(edits, key=lambda edit: (edit.start, edit.end))
    coalesced: list[Edit] = []
    for edit in ordered:
        if edit.start > edit.end:
            raise ValueError(f"invalid edit: {edit}")
        if coalesced and edit.start == edit.end == coalesced[-1].start == coalesced[-1].end:
            previous = coalesced.pop()
            coalesced.append(
                Edit(edit.start, edit.end, previous.replacement + edit.replacement)
            )
            continue
        if coalesced and edit.start < coalesced[-1].end:
            raise ValueError(f"overlapping edits: {coalesced[-1]} and {edit}")
        coalesced.append(edit)

    result = source
    for edit in reversed(coalesced):
        result = result[: edit.start] + edit.replacement + result[edit.end :]
    return result


def matching_token(tokens: Sequence[Token], open_index: int) -> int | None:
    pairs = {"(": ")", "[": "]", "{": "}", "<": ">"}
    opener = tokens[open_index].text
    closer = pairs.get(opener)
    if closer is None:
        return None
    depth = 0
    for index in range(open_index, len(tokens)):
        text = tokens[index].text
        if text == opener:
            depth += 1
        elif text == closer:
            depth -= 1
            if depth == 0:
                return index
    return None


def assembly_token_indexes(tokens: Sequence[Token]) -> set[int]:
    """Return token indexes belonging to embedded Yul assembly blocks."""

    result: set[int] = set()
    for index, token in enumerate(tokens):
        if token.text != "assembly":
            continue
        open_assembly = index + 1
        while (
            open_assembly < len(tokens)
            and tokens[open_assembly].text not in {"{", ";", "}"}
        ):
            open_assembly += 1
        if (
            open_assembly >= len(tokens)
            or tokens[open_assembly].text != "{"
        ):
            continue
        close_assembly = matching_token(tokens, open_assembly)
        if close_assembly is not None:
            result.update(range(open_assembly, close_assembly + 1))
    return result


def split_top_level(
    tokens: Sequence[Token], start: int, end: int, separator: str = ","
) -> list[tuple[int, int]]:
    result: list[tuple[int, int]] = []
    segment_start = start
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "<": ">"}
    closing = frozenset(pairs.values())

    for index in range(start, end):
        text = tokens[index].text
        if text in pairs:
            stack.append(pairs[text])
        elif text in closing and stack and text == stack[-1]:
            stack.pop()
        elif text == separator and not stack:
            result.append((segment_start, index))
            segment_start = index + 1
    result.append((segment_start, end))
    return [(left, right) for left, right in result if left < right]


class TypeParser:
    """Parse source types and render the grammar of solcore-rs/new-syntax."""

    def __init__(self, tokens: Sequence[Token], *, arrays: bool = True):
        self.tokens = tokens
        self.arrays = arrays

    def at(self, i: int) -> str:
        return self.tokens[i].text if i < len(self.tokens) else ""

    def parse(self, i: int) -> tuple[str, int] | None:
        start = i
        token = self.at(i)
        if token in {"@", "comptime"}:
            if token == "comptime" and self.at(i + 1) == "<":
                inner = self.parse(i + 2)
                if inner is None or self.at(inner[1]) != ">":
                    return None
                rendered, i = f"comptime<{inner[0]}>", inner[1] + 1
            else:
                inner = self.parse(i + 1)
                if inner is None:
                    return None
                rendered, i = (f"@{inner[0]}" if token == "@" else f"comptime<{inner[0]}>"), inner[1]
        elif token == "(":
            close = matching_token(self.tokens, i)
            if close is None:
                return None
            parts = self.type_list(i + 1, close)
            if parts is None:
                return None
            rendered, i = "(" + ", ".join(parts) + ")", close + 1
        elif token == "function" and self.at(i + 1) == "(":
            close = matching_token(self.tokens, i + 1)
            if close is None:
                return None
            parts = self.type_list(i + 2, close)
            if parts is None:
                return None
            rendered, i = "function(" + ", ".join(parts) + ")", close + 1
            while self.at(i) in {"internal", "external", "pure", "view", "payable"}:
                i += 1
            if self.at(i) == "returns" and self.at(i + 1) == "(":
                close = matching_token(self.tokens, i + 1)
                if close is None:
                    return None
                parts = self.type_list(i + 2, close)
                if parts is None:
                    return None
                rendered += " returns (" + ", ".join(parts) + ")"
                i = close + 1
        elif i < len(self.tokens) and self.tokens[i].kind == "ident":
            rendered, i = token, i + 1
            while self.at(i) == "." and i + 1 < len(self.tokens) and self.tokens[i + 1].kind == "ident":
                rendered += "." + self.at(i + 1)
                i += 2
            if rendered == "mapping" and self.at(i) == "(":
                key = self.parse(i + 1)
                if key is None or self.at(key[1]) != "=>":
                    return None
                value = self.parse(key[1] + 1)
                if value is None or self.at(value[1]) != ")":
                    return None
                rendered, i = f"mapping({key[0]} => {value[0]})", value[1] + 1
            elif self.at(i) == "<":
                close = matching_token(self.tokens, i)
                if close is None:
                    return None
                parts = self.type_list(i + 1, close)
                if parts is None:
                    return None
                rendered += "<" + ", ".join(parts) + ">"
                i = close + 1
        else:
            return None

        while i < len(self.tokens):
            if self.at(i) in {"memory", "storage", "calldata"} and self.at(i + 1) != "<":
                rendered, i = f"{self.at(i)}<{rendered}>", i + 1
            elif self.arrays and self.at(i) == "[":
                close = matching_token(self.tokens, i)
                if close is None:
                    return None
                size = "".join(t.text for t in self.tokens[i + 1:close])
                rendered = f"array<{size}, {rendered}>" if size else f"array<{rendered}>"
                i = close + 1
            else:
                break
        return rendered, i

    def type_list(self, start: int, end: int) -> list[str] | None:
        result = []
        for left, right in split_top_level(self.tokens, start, end):
            parsed = TypeParser(self.tokens).parse(left)
            if parsed is None or parsed[1] != right:
                return None
            result.append(parsed[0])
        return result


def normalize_type_fragment(source: str) -> str:
    ts = significant(source)
    parsed = TypeParser(ts).parse(0)
    return parsed[0] if parsed and parsed[1] == len(ts) else source.strip()


def code_edits(source: str, replacements: dict[str, str]) -> str:
    ts = significant(source)
    assembly = assembly_token_indexes(ts)
    return apply_edits(source, [Edit(t.start, t.end, replacements[t.text])
                                for i, t in enumerate(ts)
                                if i not in assembly and t.kind == "ident" and t.text in replacements])


def transform_pragmas(source: str) -> str:
    ts = significant(source)
    edits = []
    names = {"noPattersonCondition": "no-patterson-condition",
             "noBoundVariableCondition": "no-bounded-variable-condition",
             "noCoverageCondition": "no-coverage-condition",
             "noGenericInstanceFor": "no-generic-instance-for"}
    for i, t in enumerate(ts):
        if t.text != "pragma":
            continue
        j = i + 1
        if j < len(ts) and ts[j].text == "solcore":
            edits.append(Edit(ts[j].start, ts[j].end, ""))
            j += 1
        if j < len(ts) and ts[j].text in names:
            edits.append(Edit(ts[j].start, ts[j].end, names[ts[j].text]))
    return apply_edits(source, edits)


def transform_imports(source: str) -> str:
    ts = significant(source)
    edits = []
    for i, token in enumerate(ts):
        if token.text == "import" and [t.text for t in ts[i + 1:i + 4]] == ["{", "*", "}"]:
            edits.append(Edit(ts[i + 1].start, ts[i + 3].end, "*"))
    return apply_edits(source, edits)


def transform_aliases(source: str) -> str:
    ts = significant(source)
    assembly = assembly_token_indexes(ts)
    edits = []
    for i, t in enumerate(ts):
        if i in assembly or t.text != "alias":
            continue
        edits.append(Edit(t.start, t.end, "type"))
        if i + 2 < len(ts) and ts[i + 2].text == "<":
            close = matching_token(ts, i + 2)
            if close is not None:
                edits.extend([Edit(ts[i + 2].start, ts[i + 2].end, "("),
                              Edit(ts[close].start, ts[close].end, ")")])
    return apply_edits(source, edits)


def transform_types(source: str) -> str:
    """Normalize complete type positions, preserving expression indexes."""
    ts = significant(source)
    assembly = assembly_token_indexes(ts)
    contexts = set()
    for i, t in enumerate(ts):
        if i in assembly:
            continue
        if t.text in {":", "as", "->"}:
            contexts.add(i + 1)
        if t.text == "returns" and i + 1 < len(ts) and ts[i + 1].text == "(":
            contexts.add(i + 1)
        if t.text == "type":
            j = i + 1
            while j < len(ts) and ts[j].text not in {"=", ";", "{"}:
                j += 1
            if j < len(ts) and ts[j].text == "=":
                contexts.add(j + 1)
    edits = []
    cursor = 0
    for i, t in enumerate(ts):
        if i < cursor or i in assembly:
            continue
        if t.text in {"where", "return", "let", "case", "if", "else", "import", "export", "impl", "enum", "trait", "contract", "type", "pragma"}:
            continue
        # Generic types and location suffixes are also found in enum payloads,
        # impl heads and trait predicates. Array suffixes need a type context.
        parsed = TypeParser(ts, arrays=i in contexts).parse(i)
        if parsed is None:
            continue
        rendered, end = parsed
        if end <= i:
            continue
        fragment = source[t.start:ts[end - 1].end]
        candidate = i in contexts or any(x.text in {"memory", "storage", "calldata", "function"}
                                         for x in ts[i:end]) or ("<" in fragment and "[" in fragment)
        if candidate and normalize_token_spacing(fragment) != normalize_token_spacing(rendered):
            # Type rendering does not discard comments embedded inside a type.
            if any(x.kind == "comment" for x in tokenize(fragment)):
                continue
            edits.append(Edit(t.start, ts[end - 1].end, rendered))
            cursor = end
    return apply_edits(source, edits)


def normalize_token_spacing(source: str) -> str:
    return " ".join(t.text for t in significant(source))


def transform_comptime_lets(source: str) -> str:
    ts = significant(source)
    assembly = assembly_token_indexes(ts)
    edits = []
    for i, t in enumerate(ts):
        if i in assembly or t.text != "let" or i + 3 >= len(ts) or ts[i + 1].text != "comptime":
            continue
        j = i + 2
        if ts[j].text == "(":
            close = matching_token(ts, j)
            if close is None:
                continue
            j = close + 1
        else:
            j += 1
        if j < len(ts) and ts[j].text == ":":
            parsed = TypeParser(ts).parse(j + 1)
            if parsed:
                ty, end = parsed
                edits.append(Edit(ts[i + 1].start, ts[i + 2].start, ""))
                edits.append(Edit(ts[j + 1].start, ts[end - 1].end, f"comptime<{ty}>"))
    return apply_edits(source, edits)


def transform_lambda_returns(source: str) -> str:
    ts = significant(source)
    assembly = assembly_token_indexes(ts)
    edits = []
    for i, t in enumerate(ts):
        if i in assembly or t.text != "lam" or i + 1 >= len(ts) or ts[i + 1].text != "(":
            continue
        close = matching_token(ts, i + 1)
        if close is None or close + 2 >= len(ts) or ts[close + 1].text != "returns" or ts[close + 2].text != "(":
            continue
        end = matching_token(ts, close + 2)
        if end is None:
            continue
        items = TypeParser(ts).type_list(close + 3, end)
        if items is None:
            continue
        ty = items[0] if len(items) == 1 else "(" + ", ".join(items) + ")"
        edits.append(Edit(ts[close + 1].start, ts[end].end, "-> " + ty))
    return apply_edits(source, edits)


def module_statement_indexes(ts: Sequence[Token]) -> set[int]:
    result = set()
    for i, t in enumerate(ts):
        if t.text not in {"import", "export"}:
            continue
        j = i
        while j < len(ts) and ts[j].text != ";":
            result.add(j)
            j += 1
    return result


def transform_annotations(source: str) -> str:
    """Preserve an old expected-type annotation with a typed identity lambda."""
    for _ in range(10000):
        ts = significant(source)
        protected = assembly_token_indexes(ts) | module_statement_indexes(ts)
        found = False
        reverse_pairs = {}
        stack = []
        for i, t in enumerate(ts):
            if t.text in {"(", "[", "{"}:
                stack.append(i)
            elif t.text in {")", "]", "}"} and stack:
                wanted = {")": "(", "]": "[", "}": "{"}[t.text]
                if ts[stack[-1]].text == wanted:
                    reverse_pairs[i] = stack.pop()
        for i, t in enumerate(ts):
            if i in protected or t.text != "as" or i == 0:
                continue
            parsed = TypeParser(ts).parse(i + 1)
            if parsed is None:
                continue
            ty, end = parsed
            left = i - 1
            while left >= 0:
                if left in reverse_pairs:
                    left = reverse_pairs[left] - 1
                    continue
                if ts[left].text in {"(", "[", "{", ",", ";", "=", "return", "=>", "case"}:
                    break
                left -= 1
            left += 1
            if left == i:
                continue
            value = source[ts[left].start:t.start].strip()
            if value == "Proxy" and ty.startswith("Proxy<") and ty.endswith(">"):
                replacement = "@" + ty[6:-1]
            else:
                replacement = f"(lam (syntaxValue: {ty}) -> {ty} {{ return syntaxValue; }})({value})"
            source = apply_edits(source, [Edit(ts[left].start, ts[end - 1].end, replacement)])
            found = True
            break
        if not found:
            return source
    raise ValueError("too many expression annotations")


def hoist_annotation_bindings(source: str) -> str:
    """Give annotations local type context without introducing generic closures.

    Only unconditional argument prefixes are hoisted. A previous completed
    call, a short-circuit operand or a loop header keeps the application in
    place so evaluation order and frequency remain unchanged.
    """
    next_name = 1
    for _ in range(10000):
        ts = significant(source)
        pairs = {}
        backwards = {}
        stack = []
        for i, token in enumerate(ts):
            if token.text in {"(", "[", "{"}:
                stack.append(i)
            elif token.text in {")", "]", "}"} and stack:
                opener = {"}": "{", ")": "(", "]": "["}[token.text]
                if ts[stack[-1]].text == opener:
                    left = stack.pop()
                    pairs[left] = i
                    backwards[i] = left
        changed = False
        for i in range(len(ts) - 10):
            if [t.text for t in ts[i:i + 5]] != ["(", "lam", "(", "syntaxValue", ":"]:
                continue
            params_end = pairs.get(i + 2)
            lambda_end = pairs.get(i)
            if params_end is None or lambda_end is None or lambda_end + 1 not in pairs:
                continue
            if ts[lambda_end + 1].text != "(":
                continue
            body_start = next((j for j in range(params_end + 1, lambda_end) if ts[j].text == "{"), None)
            if body_start is None:
                continue
            body_end = pairs.get(body_start)
            if body_end is None or [t.text for t in ts[body_start + 1:body_end]] != ["return", "syntaxValue", ";"]:
                continue
            argument_end = pairs[lambda_end + 1]
            statement_start = i - 1
            while statement_start >= 0:
                token = ts[statement_start].text
                if token in {";", "{", "}"}:
                    break
                if statement_start in backwards:
                    statement_start = backwards[statement_start] - 1
                else:
                    statement_start -= 1
            statement_start += 1
            prefix = [t.text for t in ts[statement_start:i]]
            if any(t in {"&&", "||", "?", "while", "for", "else", ")"} for t in prefix):
                continue
            if "if" in prefix and (not prefix or prefix[0] != "if"):
                continue
            # A semicolon inside a for header is not a statement boundary.
            enclosing = [j for j, end in pairs.items() if j < i < end and ts[j].text == "("]
            if any(j and ts[j - 1].text in {"for", "while"} for j in enclosing):
                continue
            ty = source[ts[i + 5].start:ts[params_end].start].strip()
            value = source[ts[lambda_end + 1].end:ts[argument_end].start].strip()
            names = {t.text for t in ts if t.kind == "ident"}
            while f"syntaxValue{next_name}" in names:
                next_name += 1
            name = f"syntaxValue{next_name}"
            next_name += 1
            start = ts[statement_start].start
            line_start = source.rfind("\n", 0, start) + 1
            indentation = source[line_start:start]
            separator = "\n" + indentation if indentation.strip() == "" else " "
            binding = f"let {name}: {ty} = {value};" + separator
            source = apply_edits(source, [Edit(start, start, binding),
                                          Edit(ts[i].start, ts[argument_end].end, name)])
            changed = True
            break
        if not changed:
            return source
    raise ValueError("too many annotation bindings")


def transform_identifiers(source: str) -> str:
    ts = significant(source)
    protected = assembly_token_indexes(ts)
    for i, token in enumerate(ts):
        if token.text == "pragma":
            while i < len(ts) and ts[i].text != ";":
                protected.add(i)
                i += 1
    edits = []
    for i, t in enumerate(ts):
        if i in protected:
            continue
        if t.text == "-" and i and i + 1 < len(ts) and ts[i - 1].kind == ts[i + 1].kind == "ident":
            if ts[i - 1].end == t.start and t.end == ts[i + 1].start:
                edits.append(Edit(t.start, t.end, " - "))
            continue
        if t.kind != "ident":
            continue
        if t.text.startswith("_") and t.text != "_":
            edits.append(Edit(t.start, t.end, "syntax" + t.text))
        elif t.text == "data":
            edits.append(Edit(t.start, t.end, "dataValue"))
    return apply_edits(source, edits)


def migrate_source(source: str) -> str:
    for transform in (transform_imports, transform_pragmas, transform_aliases, transform_comptime_lets,
                      transform_types, transform_lambda_returns, transform_annotations, hoist_annotation_bindings,
                      transform_identifiers):
        source = transform(source)
    return source


def apply_file_fixups(relative: pathlib.Path, source: str) -> str:
    return source


def symlink_component(relative: pathlib.Path) -> pathlib.Path | None:
    """Return the first symlink in a repository-relative source path."""

    if relative.is_absolute() or not relative.parts or ".." in relative.parts:
        raise ValueError(f"refusing unsafe source path: {relative}")
    current = REPO_ROOT
    for part in relative.parts:
        current /= part
        if current.is_symlink():
            return current
    return None


def require_symlink_free_source(relative: pathlib.Path) -> None:
    component = symlink_component(relative)
    if component is None:
        return
    try:
        display_component = component.relative_to(REPO_ROOT)
    except ValueError:
        display_component = component
    raise ValueError(
        f"refusing to migrate symlink source: {relative} "
        f"(symlink component: {display_component})"
    )


def open_source_without_symlinks(relative: pathlib.Path, flags: int) -> int:
    """Open a source through directory descriptors without following links."""

    require_symlink_free_source(relative)
    nofollow = getattr(os, "O_NOFOLLOW", 0)
    directory = getattr(os, "O_DIRECTORY", 0)
    directory_fds: list[int] = []
    try:
        current_fd = os.open(
            REPO_ROOT,
            os.O_RDONLY | directory | nofollow,
        )
        directory_fds.append(current_fd)
        for part in relative.parts[:-1]:
            current_fd = os.open(
                part,
                os.O_RDONLY | directory | nofollow,
                dir_fd=current_fd,
            )
            directory_fds.append(current_fd)
        return os.open(
            relative.parts[-1],
            flags | nofollow,
            dir_fd=current_fd,
        )
    except OSError as exc:
        raise ValueError(
            f"refusing to access source through an unsafe worktree path: "
            f"{relative}: {exc}"
        ) from exc
    finally:
        for directory_fd in reversed(directory_fds):
            os.close(directory_fd)


def read_worktree_source(relative: pathlib.Path) -> str:
    descriptor = open_source_without_symlinks(relative, os.O_RDONLY)
    with os.fdopen(descriptor, encoding="utf-8") as source:
        return source.read()


def write_worktree_source(relative: pathlib.Path, source: str) -> None:
    descriptor = open_source_without_symlinks(
        relative,
        os.O_WRONLY | os.O_TRUNC,
    )
    with os.fdopen(descriptor, "w", encoding="utf-8") as destination:
        destination.write(source)


def packaged_solc_sources() -> list[pathlib.Path]:
    """Discover the Solcore corpus when VCS metadata is unavailable.

    Nix copies the repository into an isolated source tree without ``.git``.
    Restricting this fallback to the package's source/test roots avoids
    accidentally treating unrelated root-level or proof-of-concept files as
    migration inputs.
    """

    return sorted(
        path.relative_to(REPO_ROOT)
        for root_name in PACKAGED_SOLC_ROOTS
        for path in (REPO_ROOT / root_name).rglob("*.sol")
        if path.is_file() and not path.is_symlink()
        and path.relative_to(REPO_ROOT).as_posix() not in CLASSIC_SOL_FILES
    )


def tracked_core_source_origins() -> dict[pathlib.Path, pathlib.Path]:
    """Map current source names to their index names during an unstaged rename."""
    if not (REPO_ROOT / ".git").exists():
        indexed = packaged_solc_sources()
    else:
        process = subprocess.run(
            [
                "git",
                "-c",
                f"safe.directory={REPO_ROOT}",
                "ls-files",
                "-s",
                "-z",
                "--",
                "*.sol",
                "*.solc",  # Handle an unstaged extension migration.
            ],
            cwd=REPO_ROOT,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        indexed = [
            pathlib.Path(raw_path.decode("utf-8"))
            for entry in process.stdout.split(b"\0")
            if entry
            for metadata, raw_path in [entry.split(b"\t", 1)]
            if metadata.split(maxsplit=1)[0] != b"120000"
        ]
    indexed_set = set(indexed)
    origins = {}
    for original in indexed:
        if original.as_posix() in CLASSIC_SOL_FILES:
            continue
        if not (original.suffix == ".solc" or str(original).startswith(("src/", "std/", "test/"))):
            continue
        # The old corpus contained classic Solidity fib.sol beside Core
        # fib.solc. Its renamed Core source must retain the .solc HEAD origin.
        if original.suffix == ".sol" and original.with_suffix(".solc") in indexed_set:
            continue
        current = original
        original_file = REPO_ROOT / original
        if original.suffix == ".solc" and not original_file.exists() and not original_file.is_symlink():
            current = original.with_suffix(".sol")
        origins[current] = original
    for source in CORE_SOL_FILES:
        path = pathlib.Path(source)
        origins.setdefault(path, path)
    return origins


def tracked_core_sources() -> list[pathlib.Path]:
    return sorted(tracked_core_source_origins())


def eligible_paths(arguments: Sequence[str]) -> list[pathlib.Path]:
    allowed = frozenset(tracked_core_sources())
    if not arguments:
        result = sorted(allowed)
        for candidate in result:
            require_symlink_free_source(candidate)
        return result
    result: list[pathlib.Path] = []
    for argument in arguments:
        candidate = pathlib.Path(argument)
        if candidate.is_absolute():
            # Keep the lexical path.  Resolving first would turn an untracked
            # symlink alias into its tracked target and bypass the corpus
            # allow-list (and, with --write, modify that target).
            candidate = candidate.relative_to(REPO_ROOT)
        require_symlink_free_source(candidate)
        if candidate not in allowed:
            raise ValueError(
                f"refusing to migrate non-Core or untracked source: {candidate}"
            )
        result.append(candidate)
    return sorted(dict.fromkeys(result))


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true", help="rewrite eligible files")
    mode.add_argument(
        "--check",
        action="store_true",
        help="exit nonzero when eligible files still need migration",
    )
    parser.add_argument(
        "--from-head",
        action="store_true",
        help="with --write, regenerate eligible files from their HEAD versions",
    )
    parser.add_argument("paths", nargs="*")
    args = parser.parse_args(argv)
    if args.from_head and not args.write:
        parser.error("--from-head requires --write")

    try:
        paths = eligible_paths(args.paths)
    except (ValueError, subprocess.CalledProcessError) as error:
        parser.error(str(error))

    changed: list[pathlib.Path] = []
    origins = tracked_core_source_origins() if args.from_head else {}
    for relative in paths:
        try:
            current = read_worktree_source(relative)
        except ValueError as error:
            parser.error(str(error))
        if args.from_head:
            source = subprocess.run(
                [
                    "git",
                    "-c",
                    f"safe.directory={REPO_ROOT}",
                    "show",
                    f"HEAD:{origins.get(relative, relative).as_posix()}",
                ],
                cwd=REPO_ROOT,
                check=True,
                stdout=subprocess.PIPE,
                text=True,
            ).stdout
        else:
            source = current
        migrated = SPECIAL_FIXTURES.get(relative)
        if migrated is None:
            migrated = migrate_source(source)
        migrated = apply_file_fixups(relative, migrated)
        if migrated == current:
            continue
        changed.append(relative)
        if args.write:
            try:
                write_worktree_source(relative, migrated)
            except ValueError as error:
                parser.error(str(error))

    action = "updated" if args.write else "needs migration"
    for path in changed:
        print(f"{action}: {path}")
    print(f"{len(changed)} of {len(paths)} eligible files {action}")
    return 1 if args.check and changed else 0


if __name__ == "__main__":
    sys.exit(main())
