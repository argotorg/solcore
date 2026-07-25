#!/usr/bin/env python3
"""Migrate the tracked Solcore source corpus to the Solidity-style syntax.

The migration is deliberately token-aware:

* comments, string literals, and Yul meta expressions are never searched or
  rewritten as source code;
* parenthesized calls are changed to angle-bracket type applications only in a
  syntactic type position;
* only git-tracked ``.solc`` files and the explicitly listed Core ``.sol``
  sources are eligible for the default corpus migration; in packaged source
  trees without ``.git`` metadata, the same corpus is discovered below the
  repository's ``src``, ``std``, and ``test`` source roots.

The unresolved export grammar and contextual ``.Constructor`` shorthand are
intentionally preserved.  Legacy proxy shorthand is migrated to
``Proxy<T>`` in types and ``Proxy as Proxy<T>`` in expressions.
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
        "test/examples/dispatch/fib.sol",
    }
)

SPECIAL_FIXTURES = {
    # Keep this diagnostic fixture parse-invalid, but make the malformed input
    # use only new-syntax tokens so the failure is reported at the real fault
    # rather than after declaration-parser backtracking.
    pathlib.Path("test/diagnostics/parse-error.solc"): (
        "enum Broken { Value(word }\n"
    ),
    # This negative trait fixture intentionally has no implementation.  Give
    # its declaration a complete new-syntax signature so regeneration from
    # HEAD preserves the original semantic failure instead of stopping at the
    # removed arrow syntax (or at a missing signature terminator).
    pathlib.Path("test/examples/cases/catenable-err.solc"): (
        "trait Catenable<t> {\n"
        "  function cat(x: t) returns (bytes memory);\n"
        "}\n"
    ),
}

FILE_FIXUPS = {
    # The old source accidentally omitted the colon on its second parameter.
    # Preserve the hand-corrected new-syntax declaration when regenerating the
    # corpus from HEAD.
    pathlib.Path("blog-post/sum.sol"): (
        (
            "function sum (p1 : (T1, T2), p2 (T1, T2)) "
            "returns ((T1, T2)) {",
            "function sum (p1 : (T1, T2), p2 : (T1, T2)) "
            "returns ((T1, T2)) {",
        ),
    ),
}

MODIFIERS = frozenset(
    {"public", "private", "external", "internal", "pure", "view", "payable"}
)

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
    """Small parser for the old and new surface type grammars."""

    def __init__(self, tokens: Sequence[Token]):
        self.tokens = tokens

    def parse(self, index: int) -> tuple[str, int] | None:
        left = self.parse_atom(index)
        if left is None:
            return None
        rendered, index = left
        if index < len(self.tokens) and self.tokens[index].text == "->":
            right = self.parse(index + 1)
            if right is None:
                return None
            right_text, index = right
            rendered = f"function({rendered}) internal returns ({right_text})"
        return rendered, index

    def parse_atom(self, index: int) -> tuple[str, int] | None:
        if index >= len(self.tokens):
            return None

        token = self.tokens[index]
        text = token.text

        if text == "comptime":
            parsed = self.parse_atom(index + 1)
            if parsed is None:
                return None
            rendered, index = parsed
            return f"comptime {rendered}", index

        if text == "@":
            parsed = self.parse_atom(index + 1)
            if parsed is None:
                return None
            rendered, index = parsed
            return f"Proxy<{rendered}>", index

        if text == "(":
            close = matching_token(self.tokens, index)
            if close is None:
                return None
            if close == index + 1:
                rendered = "()"
            else:
                parts: list[str] = []
                for left, right in split_top_level(self.tokens, index + 1, close):
                    parsed = self.parse(left)
                    if parsed is None or parsed[1] != right:
                        return None
                    parts.append(parsed[0])
                rendered = parts[0] if len(parts) == 1 else f"({', '.join(parts)})"
            return self.parse_array_suffix(rendered, close + 1)

        if token.kind != "ident":
            return None

        name_parts = [text]
        index += 1
        while (
            index + 1 < len(self.tokens)
            and self.tokens[index].text == "."
            and self.tokens[index + 1].kind == "ident"
        ):
            name_parts.extend((".", self.tokens[index + 1].text))
            index += 2
        name = "".join(name_parts)

        if name == "function" and index < len(self.tokens) and self.tokens[index].text == "(":
            close = matching_token(self.tokens, index)
            if close is None:
                return None
            args = self.parse_type_list(index + 1, close)
            if args is None:
                return None
            index = close + 1
            attributes: list[str] = []
            while index < len(self.tokens) and self.tokens[index].text in {
                "internal",
                "external",
                "pure",
                "view",
                "payable",
            }:
                attributes.append(self.tokens[index].text)
                index += 1
            if index >= len(self.tokens) or self.tokens[index].text != "returns":
                return None
            if index + 1 >= len(self.tokens) or self.tokens[index + 1].text != "(":
                return None
            ret_close = matching_token(self.tokens, index + 1)
            if ret_close is None:
                return None
            returns = self.parse_type_list(index + 2, ret_close)
            if returns is None:
                return None
            attr_text = (
                " " + " ".join(attributes)
                if attributes
                else ""
            )
            rendered = (
                f"function({', '.join(args)}){attr_text} "
                f"returns ({', '.join(returns)})"
            )
            return self.parse_array_suffix(rendered, ret_close + 1)

        has_old_bracket_args = (
            index < len(self.tokens)
            and self.tokens[index].text == "["
            and name in {"Memory", "Stack", "Ref"}
        )
        if (
            index < len(self.tokens)
            and self.tokens[index].text in {"(", "<"}
        ) or has_old_bracket_args:
            opener = self.tokens[index].text
            close = matching_token(self.tokens, index)
            if close is None:
                return None
            args = self.parse_type_list(index + 1, close)
            if args is None:
                return None
            if name == "mapping" and len(args) == 2:
                rendered = f"mapping({args[0]} => {args[1]})"
            elif name in {"memory", "storage", "calldata"} and len(args) == 1:
                rendered = f"{args[0]} {name}"
            elif name == "array" and len(args) == 1:
                rendered = f"{args[0]}[]"
            elif name == "array" and len(args) == 2:
                rendered = f"{args[1]}[{args[0]}]"
            else:
                rendered = f"{name}<{', '.join(args)}>"
            index = close + 1
        else:
            rendered = name

        return self.parse_array_suffix(rendered, index)

    def parse_array_suffix(self, rendered: str, index: int) -> tuple[str, int]:
        while index < len(self.tokens) and self.tokens[index].text == "[":
            close = matching_token(self.tokens, index)
            if close is None:
                break
            size = "".join(token.text for token in self.tokens[index + 1 : close])
            rendered += f"[{size}]"
            index = close + 1
        return rendered, index

    def parse_type_list(self, start: int, end: int) -> list[str] | None:
        if start == end:
            return []
        rendered: list[str] = []
        for left, right in split_top_level(self.tokens, start, end):
            parsed = self.parse(left)
            if parsed is None or parsed[1] != right:
                return None
            rendered.append(parsed[0])
        return rendered


def normalize_type_fragment(source: str) -> str:
    tokens = significant(source)
    if not tokens:
        return source.strip()
    parsed = TypeParser(tokens).parse(0)
    if parsed is None or parsed[1] != len(tokens):
        return "".join(token.text for token in tokens)
    return parsed[0]


def normalize_predicate_fragment(source: str) -> str:
    tokens = significant(source.strip())
    if tokens and tokens[0].text == "(":
        close = matching_token(tokens, 0)
        if close == len(tokens) - 1:
            tokens = tokens[1:close]

    colon = top_level_token(tokens, ":")
    if colon is None:
        return "".join(token.text for token in tokens)

    subject_tokens = tokens[:colon]
    class_tokens = tokens[colon + 1 :]
    subject = TypeParser(subject_tokens).parse(0)
    if subject is None or subject[1] != len(subject_tokens):
        subject_text = "".join(token.text for token in subject_tokens)
    else:
        subject_text = subject[0]

    if not class_tokens:
        return f"{subject_text}:"

    name_parts: list[str] = []
    index = 0
    while index < len(class_tokens):
        if class_tokens[index].kind != "ident":
            break
        name_parts.append(class_tokens[index].text)
        index += 1
        if (
            index + 1 < len(class_tokens)
            and class_tokens[index].text == "."
            and class_tokens[index + 1].kind == "ident"
        ):
            name_parts.append(".")
            index += 1
            continue
        break
    class_name = "".join(name_parts)
    params: list[str] = []
    if index < len(class_tokens) and class_tokens[index].text in {"(", "[", "<"}:
        close = matching_token(class_tokens, index)
        if close is not None:
            parsed_params = TypeParser(class_tokens).parse_type_list(index + 1, close)
            if parsed_params is not None:
                params = parsed_params
                index = close + 1
    class_text = class_name
    if params:
        class_text += f"<{', '.join(params)}>"
    if index < len(class_tokens):
        class_text += "".join(token.text for token in class_tokens[index:])
    return f"{subject_text}: {class_text}"


def normalize_predicate_list(source: str) -> str:
    tokens = significant(source)
    if not tokens:
        return ""
    if tokens[0].text == "(":
        close = matching_token(tokens, 0)
        if close == len(tokens) - 1:
            tokens = tokens[1:close]
    predicates: list[str] = []
    for left, right in split_top_level(tokens, 0, len(tokens)):
        fragment_start = tokens[left].start
        fragment_end = tokens[right - 1].end
        predicates.append(
            normalize_predicate_fragment(source[fragment_start:fragment_end])
        )
    return ", ".join(predicate for predicate in predicates if predicate)


def top_level_token(tokens: Sequence[Token], wanted: str) -> int | None:
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "<": ">"}
    for index, token in enumerate(tokens):
        text = token.text
        if text in pairs:
            stack.append(pairs[text])
        elif stack and text == stack[-1]:
            stack.pop()
        elif not stack and text == wanted:
            return index
    return None


def preserved_comments(source: str, start: int, end: int) -> str:
    comments = [
        token.text
        for token in tokenize(source[start:end])
        if token.kind == "comment"
    ]
    if not comments:
        return ""
    return "\n".join(comments) + "\n"


def declaration_end(
    source: str, tokens: Sequence[Token], start_index: int
) -> tuple[int, bool]:
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "<": ">"}
    for index in range(start_index, len(tokens)):
        text = tokens[index].text
        if text in pairs:
            stack.append(pairs[text])
        elif stack and text == stack[-1]:
            stack.pop()
        elif not stack and text == ";":
            return index, True
        elif not stack and text in {"{", "}"}:
            break

    line_end = source.find("\n", tokens[start_index].start)
    if line_end < 0:
        line_end = len(source)
    last = start_index
    while last + 1 < len(tokens) and tokens[last + 1].start < line_end:
        last += 1
    return last, False


def transform_imports(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text != "import":
            continue
        if (
            index + 1 >= len(tokens)
            or tokens[index + 1].text in {"{", "*"}
        ):
            continue
        end = index + 1
        while end < len(tokens) and tokens[end].text != ";":
            end += 1
        if end >= len(tokens):
            continue

        dot_brace = None
        stack: list[str] = []
        for cursor in range(index + 1, end):
            text = tokens[cursor].text
            if text in {"(", "[", "<"}:
                stack.append({"(": ")", "[": "]", "<": ">"}[text])
            elif stack and text == stack[-1]:
                stack.pop()
            elif (
                not stack
                and text == "."
                and cursor + 1 < end
                and tokens[cursor + 1].text == "{"
            ):
                dot_brace = cursor
                break

        if dot_brace is not None:
            brace = dot_brace + 1
            close = matching_token(tokens, brace)
            if close is None or close > end:
                continue
            path = source[tokens[index + 1].start : tokens[dot_brace].start].strip()
            selection = source[tokens[brace].start : tokens[close].end]
            tail = source[tokens[close].end : tokens[end].start].strip()
            replacement = f"import {selection} from {path}"
            if tail:
                replacement += f" {tail}"
            replacement += ";"
            edits.append(Edit(token.start, tokens[end].end, replacement))
            continue

        alias = None
        for cursor in range(index + 1, end):
            if tokens[cursor].text == "as":
                alias = cursor
                break
        if alias is not None and alias + 1 < end:
            path = source[tokens[index + 1].start : tokens[alias].start].strip()
            alias_name = tokens[alias + 1].text
            replacement = f"import * as {alias_name} from {path};"
            edits.append(Edit(token.start, tokens[end].end, replacement))
    return apply_edits(source, edits)


def transform_pragmas(source: str) -> str:
    replacements = {
        ("no", "-", "coverage", "-", "condition"): "solcore noCoverageCondition",
        ("no", "-", "patterson", "-", "condition"): "solcore noPattersonCondition",
        (
            "no",
            "-",
            "bounded",
            "-",
            "variable",
            "-",
            "condition",
        ): "solcore noBoundVariableCondition",
        (
            "no",
            "-",
            "generic",
            "-",
            "instance",
            "-",
            "for",
        ): "solcore noGenericInstanceFor",
    }
    tokens = significant(source)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text != "pragma":
            continue
        for parts, replacement in replacements.items():
            candidate = tuple(
                item.text for item in tokens[index + 1 : index + 1 + len(parts)]
            )
            if candidate == parts:
                edits.append(
                    Edit(
                        tokens[index + 1].start,
                        tokens[index + len(parts)].end,
                        replacement,
                    )
                )
                break
    return apply_edits(source, edits)


def transform_data_declarations(source: str) -> str:
    tokens = significant(source)
    yul = assembly_token_indexes(tokens)
    scopes: list[tuple[str, ...]] = []
    scope_stack: list[str] = []
    for token_index, token in enumerate(tokens):
        scopes.append(tuple(scope_stack))
        if token.text == "{":
            scope_stack.append(brace_header_kind(tokens, token_index))
        elif token.text == "}" and scope_stack:
            scope_stack.pop()

    edits: list[Edit] = []
    index = 0
    while index < len(tokens):
        if (
            tokens[index].text != "data"
            or index in yul
            or index + 1 >= len(tokens)
            or tokens[index + 1].kind != "ident"
        ):
            index += 1
            continue

        # ``data`` is no longer reserved, so only recognize the old
        # declaration at top level or directly inside a contract.  In
        # particular, do not reinterpret parameters, fields, locals, member
        # access, or Yul identifiers named ``data``.
        scope = scopes[index]
        if scope not in {(), ("contract",)}:
            index += 1
            continue
        at_declaration_boundary = index == 0
        if index > 0:
            previous = tokens[index - 1]
            gap = source[previous.end : tokens[index].start]
            at_declaration_boundary = (
                previous.text in {"{", "}", ";"} or "\n" in gap
            )
        if not at_declaration_boundary:
            index += 1
            continue

        start = index
        name = tokens[index + 1].text
        cursor = index + 2
        params: list[str] = []
        if cursor < len(tokens) and tokens[cursor].text in {"(", "[", "<"}:
            close = matching_token(tokens, cursor)
            if close is None:
                index += 1
                continue
            parsed = TypeParser(tokens).parse_type_list(cursor + 1, close)
            if parsed is None:
                index += 1
                continue
            params = parsed
            cursor = close + 1

        if (
            cursor >= len(tokens)
            or tokens[cursor].text not in {"=", ";"}
        ):
            index += 1
            continue

        end_index, had_semicolon = declaration_end(source, tokens, cursor)
        equals = None
        for probe in range(cursor, end_index + 1):
            if tokens[probe].text == "=":
                equals = probe
                break

        constructors: list[str] = []
        if equals is not None:
            constructor_end = end_index if had_semicolon else end_index + 1
            for left, right in split_top_level(tokens, equals + 1, constructor_end, "|"):
                if left >= right or tokens[left].kind != "ident":
                    continue
                constructor = tokens[left].text
                payload_index = left + 1
                if payload_index < right and tokens[payload_index].text == "(":
                    close = matching_token(tokens, payload_index)
                    if close is None or close >= right + 1:
                        continue
                    payload = TypeParser(tokens).parse_type_list(
                        payload_index + 1, close
                    )
                    if payload is None:
                        continue
                    constructor += f"({', '.join(payload)})"
                constructors.append(constructor)

        params_text = f"<{', '.join(params)}>" if params else ""
        replacement = f"enum {name}{params_text} {{"
        if constructors:
            replacement += f" {', '.join(constructors)} "
        replacement += "}"
        replacement = (
            preserved_comments(
                source, tokens[start].start, tokens[end_index].end
            )
            + replacement
        )
        edits.append(
            Edit(tokens[start].start, tokens[end_index].end, replacement)
        )
        index = end_index + 1
    return apply_edits(source, edits)


def transform_type_declarations(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    index = 0
    while index < len(tokens):
        if tokens[index].text != "type" or index + 1 >= len(tokens):
            index += 1
            continue
        start = index
        name = tokens[index + 1].text
        cursor = index + 2
        params: list[str] = []
        if cursor < len(tokens) and tokens[cursor].text in {"(", "[", "<"}:
            close = matching_token(tokens, cursor)
            if close is None:
                index += 1
                continue
            parsed = TypeParser(tokens).parse_type_list(cursor + 1, close)
            if parsed is None:
                index += 1
                continue
            params = parsed
            cursor = close + 1
        # ``type Name is Type`` is the new nominal user-defined-value-type
        # spelling.  It must remain untouched even while compiler support is
        # pending.  Only the legacy transparent ``type Name = Type`` form is
        # migrated to ``alias``.
        if cursor >= len(tokens) or tokens[cursor].text != "=":
            index += 1
            continue
        end_index, _ = declaration_end(source, tokens, cursor + 1)
        rhs_end = end_index if tokens[end_index].text == ";" else end_index + 1
        parsed_rhs = TypeParser(tokens).parse(cursor + 1)
        if parsed_rhs is None or parsed_rhs[1] != rhs_end:
            index += 1
            continue
        params_text = f"<{', '.join(params)}>" if params else ""
        replacement = f"alias {name}{params_text} = {parsed_rhs[0]};"
        replacement = (
            preserved_comments(
                source, tokens[start].start, tokens[end_index].end
            )
            + replacement
        )
        edits.append(
            Edit(tokens[start].start, tokens[end_index].end, replacement)
        )
        index = end_index + 1
    return apply_edits(source, edits)


@dataclasses.dataclass(frozen=True)
class SignaturePrefix:
    start: int
    end: int
    variables: tuple[str, ...]
    context: str


def declaration_boundary(tokens: Sequence[Token], index: int) -> int:
    cursor = index - 1
    while cursor >= 0:
        if tokens[cursor].text in {"{", "}", ";"}:
            return cursor + 1
        cursor -= 1
    return 0


def infer_constraint_variables(context: str) -> tuple[str, ...]:
    concrete = {
        "word",
        "bool",
        "integer",
        "string",
        "bytes",
        "address",
        "memory",
        "storage",
        "calldata",
        "returndata",
        "mapping",
        "array",
        "pair",
        "sum",
        "function",
        "comptime",
    }
    result: list[str] = []
    tokens = significant(context)
    for index, token in enumerate(tokens):
        if token.kind != "ident" or token.text in concrete:
            continue
        if token.text[0].isupper():
            continue
        if index > 0 and tokens[index - 1].text in {":", "."}:
            continue
        if token.text not in result:
            result.append(token.text)
    return tuple(result)


def find_signature_prefix(
    source: str, tokens: Sequence[Token], keyword_index: int
) -> SignaturePrefix | None:
    boundary = declaration_boundary(tokens, keyword_index)
    forall = None
    for index in range(boundary, keyword_index):
        if tokens[index].text == "forall":
            forall = index
            break
    if forall is None:
        return None

    dot = None
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "<": ">"}
    for index in range(forall + 1, keyword_index):
        text = tokens[index].text
        if text in pairs:
            stack.append(pairs[text])
        elif stack and text == stack[-1]:
            stack.pop()
        elif not stack and text == ".":
            dot = index
            break
    if dot is None:
        return None

    arrow = None
    stack.clear()
    for index in range(dot + 1, keyword_index):
        text = tokens[index].text
        if text in pairs:
            stack.append(pairs[text])
        elif stack and text == stack[-1]:
            stack.pop()
        elif not stack and text == "=>":
            arrow = index
            break

    before = tokens[forall + 1 : dot]
    ordinary_vars = all(
        token.kind == "ident" or token.text == "," for token in before
    )
    if ordinary_vars:
        variables = tuple(
            token.text for token in before if token.kind == "ident"
        )
        context = (
            source[tokens[dot + 1].start : tokens[arrow].start].strip()
            if arrow is not None and dot + 1 < arrow
            else ""
        )
    else:
        context_start = tokens[forall + 1].start
        context_end = tokens[dot].start
        first_context = source[context_start:context_end].strip()
        second_context = (
            source[tokens[dot + 1].start : tokens[arrow].start].strip()
            if arrow is not None and dot + 1 < arrow
            else ""
        )
        context = ", ".join(
            fragment for fragment in (first_context, second_context) if fragment
        )
        variables = infer_constraint_variables(context)

    end_index = arrow if arrow is not None else dot
    return SignaturePrefix(
        start=tokens[forall].start,
        end=tokens[end_index].end,
        variables=variables,
        context=normalize_predicate_list(context) if context else "",
    )


def parse_qualified_name(
    tokens: Sequence[Token], index: int
) -> tuple[str, int] | None:
    if index >= len(tokens) or tokens[index].kind != "ident":
        return None
    parts = [tokens[index].text]
    index += 1
    while (
        index + 1 < len(tokens)
        and tokens[index].text == "."
        and tokens[index + 1].kind == "ident"
    ):
        parts.extend((".", tokens[index + 1].text))
        index += 2
    return "".join(parts), index


def transform_traits_and_impls(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    for keyword_index, token in enumerate(tokens):
        if token.text not in {"class", "instance"}:
            continue
        is_trait = token.text == "class"
        prefix = find_signature_prefix(source, tokens, keyword_index)
        start_offset = prefix.start if prefix else token.start
        variables = list(prefix.variables if prefix else ())
        contexts: list[str] = [prefix.context] if prefix and prefix.context else []

        if (
            not is_trait
            and prefix is None
            and keyword_index > 0
            and tokens[keyword_index - 1].text == "default"
        ):
            start_offset = tokens[keyword_index - 1].start

        cursor = keyword_index + 1
        if (
            not is_trait
            and cursor < len(tokens)
            and tokens[cursor].text == "("
        ):
            close = matching_token(tokens, cursor)
            if (
                close is not None
                and close + 1 < len(tokens)
                and tokens[close + 1].text == "=>"
            ):
                context_text = source[
                    tokens[cursor + 1].start : tokens[close].start
                ]
                contexts.append(normalize_predicate_list(context_text))
                cursor = close + 2

        parsed_subject = TypeParser(tokens).parse(cursor)
        if parsed_subject is None:
            continue
        subject, cursor = parsed_subject
        if cursor >= len(tokens) or tokens[cursor].text != ":":
            continue
        parsed_name = parse_qualified_name(tokens, cursor + 1)
        if parsed_name is None:
            continue
        class_name, cursor = parsed_name

        params: list[str] = []
        if cursor < len(tokens) and tokens[cursor].text in {"(", "[", "<"}:
            close = matching_token(tokens, cursor)
            if close is None:
                continue
            parsed_params = TypeParser(tokens).parse_type_list(cursor + 1, close)
            if parsed_params is None:
                continue
            params = parsed_params
            cursor = close + 1

        while cursor < len(tokens) and tokens[cursor].text not in {"{", ";"}:
            cursor += 1
        if cursor >= len(tokens):
            continue

        all_args = [subject, *params]
        args_text = f"<{', '.join(all_args)}>"
        if is_trait:
            header = f"trait {class_name}{args_text}"
        else:
            default = (
                keyword_index > 0 and tokens[keyword_index - 1].text == "default"
            )
            generic_text = f"<{', '.join(variables)}>" if variables else ""
            header = f"{'default ' if default else ''}impl{generic_text} {class_name}{args_text}"
        context = ", ".join(part for part in contexts if part)
        if context:
            header += f" where {context}"
        header += " "
        header = preserved_comments(source, start_offset, tokens[cursor].start) + header
        edits.append(Edit(start_offset, tokens[cursor].start, header))
    return apply_edits(source, edits)


def function_header_end(tokens: Sequence[Token], index: int) -> int | None:
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "<": ">"}
    for cursor in range(index, len(tokens)):
        text = tokens[cursor].text
        if text in pairs:
            stack.append(pairs[text])
        elif stack and text == stack[-1]:
            stack.pop()
        elif not stack and text in {"{", ";"}:
            return cursor
    return None


def transform_functions(source: str) -> str:
    tokens = significant(source)
    yul = assembly_token_indexes(tokens)
    edits: list[Edit] = []
    for keyword_index, token in enumerate(tokens):
        if token.text not in {"function", "constructor", "fallback", "lam"}:
            continue
        if keyword_index in yul:
            continue
        keyword = token.text
        prefix = (
            find_signature_prefix(source, tokens, keyword_index)
            if keyword == "function"
            else None
        )
        variables = list(prefix.variables if prefix else ())
        context = prefix.context if prefix else ""
        if prefix is not None:
            prefix_end = prefix.end
            first_header_token = keyword_index
            while (
                first_header_token > 0
                and tokens[first_header_token - 1].text in MODIFIERS
            ):
                first_header_token -= 1
            gap_end = tokens[first_header_token].start
            if source[prefix.end:gap_end].strip() == "":
                prefix_end = gap_end
            edits.append(
                Edit(
                    prefix.start,
                    prefix_end,
                    preserved_comments(source, prefix.start, prefix_end),
                )
            )

        modifiers: list[str] = []
        cursor = keyword_index - 1
        while cursor >= 0 and tokens[cursor].text in MODIFIERS:
            modifiers.insert(0, tokens[cursor].text)
            cursor -= 1
        if modifiers:
            modifier_start = cursor + 1
            edits.append(
                Edit(
                    tokens[modifier_start].start,
                    token.start,
                    preserved_comments(
                        source, tokens[modifier_start].start, token.start
                    ),
                )
            )

        if keyword == "function":
            if keyword_index + 1 >= len(tokens):
                continue
            name_index = keyword_index + 1
            cursor = name_index + 1
            if (
                cursor < len(tokens)
                and tokens[cursor].text == "<"
                and matching_token(tokens, cursor) is not None
            ):
                cursor = matching_token(tokens, cursor) + 1  # type: ignore[operator]
            if variables and (
                name_index + 1 >= len(tokens)
                or tokens[name_index + 1].text != "<"
            ):
                edits.append(
                    Edit(
                        tokens[name_index].end,
                        tokens[name_index].end,
                        f"<{', '.join(variables)}>",
                    )
                )
        else:
            cursor = keyword_index + 1

        if cursor >= len(tokens) or tokens[cursor].text != "(":
            continue
        close = matching_token(tokens, cursor)
        if close is None:
            continue
        header_end = function_header_end(tokens, close + 1)
        if header_end is None:
            continue

        existing_postfix_modifiers = {
            tokens[probe].text
            for probe in range(close + 1, header_end)
            if tokens[probe].text in MODIFIERS
        }
        if (
            keyword == "fallback"
            and "external" not in modifiers
            and "external" not in existing_postfix_modifiers
        ):
            modifiers.insert(0, "external")
        if modifiers:
            edits.append(
                Edit(
                    tokens[close].end,
                    tokens[close].end,
                    " " + " ".join(modifiers),
                )
            )

        arrow = None
        for probe in range(close + 1, header_end):
            if tokens[probe].text == "->":
                arrow = probe
                break
        if arrow is not None:
            type_start = arrow + 1
            return_comptime = False
            if (
                type_start < header_end
                and tokens[type_start].text == "comptime"
            ):
                return_comptime = True
                type_start += 1
            parsed_return = TypeParser(tokens).parse(type_start)
            if parsed_return is not None:
                return_type, return_end = parsed_return
                if return_end <= header_end:
                    if keyword == "fallback" and return_type == "()":
                        replacement = ""
                    else:
                        comptime = "comptime " if return_comptime else ""
                        replacement = f"returns ({comptime}{return_type})"
                    edits.append(
                        Edit(
                            tokens[arrow].start,
                            tokens[return_end - 1].end,
                            replacement,
                        )
                    )

        if context:
            edits.append(
                Edit(
                    tokens[header_end].start,
                    tokens[header_end].start,
                    f" where {context} ",
                )
            )
    return apply_edits(source, edits)


def transform_let_comptime(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if (
            token.text == "let"
            and index + 3 < len(tokens)
            and tokens[index + 1].kind == "ident"
            and tokens[index + 2].text == ":"
            and tokens[index + 3].text == "comptime"
        ):
            edits.append(
                Edit(token.end, token.end, " comptime")
            )
            edits.append(
                Edit(tokens[index + 3].start, tokens[index + 3].end, "")
            )
    return apply_edits(source, edits)


def transform_matches(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    brace_depth: list[int] = [0] * len(tokens)
    depth = 0
    for index, token in enumerate(tokens):
        brace_depth[index] = depth
        if token.text == "{":
            depth += 1
        elif token.text == "}":
            depth -= 1

    for match_index, token in enumerate(tokens):
        if token.text != "match":
            continue
        open_brace = None
        stack: list[str] = []
        pairs = {"(": ")", "[": "]", "<": ">"}
        for cursor in range(match_index + 1, len(tokens)):
            text = tokens[cursor].text
            if text in pairs:
                stack.append(pairs[text])
            elif stack and text == stack[-1]:
                stack.pop()
            elif not stack and text == "{":
                open_brace = cursor
                break
            elif not stack and text == ";":
                break
        if open_brace is None:
            continue
        close_brace = matching_token(tokens, open_brace)
        if close_brace is None:
            continue

        head_start = match_index + 1
        already_parenthesized = (
            head_start < open_brace
            and tokens[head_start].text == "("
            and matching_token(tokens, head_start) == open_brace - 1
        )
        if not already_parenthesized:
            edits.append(
                Edit(tokens[head_start].start, tokens[head_start].start, "(")
            )
            edits.append(
                Edit(tokens[open_brace].start, tokens[open_brace].start, ") ")
            )

        arm_bars: list[int] = []
        paren_depth = bracket_depth = 0
        for cursor in range(open_brace + 1, close_brace):
            text = tokens[cursor].text
            if text == "(":
                paren_depth += 1
            elif text == ")":
                paren_depth -= 1
            elif text == "[":
                bracket_depth += 1
            elif text == "]":
                bracket_depth -= 1
            elif (
                text == "|"
                and paren_depth == 0
                and bracket_depth == 0
                and brace_depth[cursor] == brace_depth[open_brace] + 1
            ):
                arm_bars.append(cursor)
        if not arm_bars:
            continue

        # In the old grammar, ``match (a, b)`` could mean matching one tuple
        # expression, while new syntax uses that spelling for two scrutinees.
        # A single old arm pattern disambiguates the former; retain it by
        # adding one more pair of parentheses.
        if already_parenthesized:
            first_bar = arm_bars[0]
            first_arm_end = (
                arm_bars[1] if len(arm_bars) > 1 else close_brace
            )
            first_arrow = next(
                (
                    cursor
                    for cursor in range(first_bar + 1, first_arm_end)
                    if tokens[cursor].text == "=>"
                ),
                None,
            )
            head_close = matching_token(tokens, head_start)
            if first_arrow is not None and head_close is not None:
                head_parts = split_top_level(
                    tokens, head_start + 1, head_close
                )
                pattern_parts = split_top_level(
                    tokens, first_bar + 1, first_arrow
                )
                if len(head_parts) > 1 and len(pattern_parts) == 1:
                    edits.append(
                        Edit(
                            tokens[head_start].start,
                            tokens[head_start].start,
                            "(",
                        )
                    )
                    edits.append(
                        Edit(
                            tokens[head_close].end,
                            tokens[head_close].end,
                            ")",
                        )
                    )

        for position, bar in enumerate(arm_bars):
            arm_end = (
                arm_bars[position + 1]
                if position + 1 < len(arm_bars)
                else close_brace
            )
            arrow = None
            stack = []
            for cursor in range(bar + 1, arm_end):
                text = tokens[cursor].text
                if text in {"(", "[", "<"}:
                    stack.append({"(": ")", "[": "]", "<": ">"}[text])
                elif stack and text == stack[-1]:
                    stack.pop()
                elif not stack and text == "=>":
                    arrow = cursor
                    break
            if arrow is None:
                continue

            pattern_ranges = split_top_level(tokens, bar + 1, arrow)
            wildcard_default = bool(pattern_ranges) and all(
                right == left + 1 and tokens[left].text == "_"
                for left, right in pattern_ranges
            )
            if wildcard_default:
                edits.append(
                    Edit(tokens[bar].start, tokens[arrow].end, "default {")
                )
            else:
                edits.append(Edit(tokens[bar].start, tokens[bar].end, "case"))
                if len(pattern_ranges) > 1:
                    edits.append(
                        Edit(
                            tokens[bar + 1].start,
                            tokens[bar + 1].start,
                            "(",
                        )
                    )
                    edits.append(
                        Edit(tokens[arrow].start, tokens[arrow].start, ") ")
                    )
                edits.append(
                    Edit(tokens[arrow].start, tokens[arrow].end, "{")
                )
            edits.append(
                Edit(tokens[arm_end].start, tokens[arm_end].start, "} ")
            )
    return apply_edits(source, edits)


def ternary_colons(tokens: Sequence[Token]) -> set[int]:
    questions: dict[tuple[int, int, int], list[int]] = {}
    paren = bracket = brace = 0
    result: set[int] = set()
    for index, token in enumerate(tokens):
        text = token.text
        scope = (paren, bracket, brace)
        if text == "?":
            questions.setdefault(scope, []).append(index)
        elif text == ":" and questions.get(scope):
            questions[scope].pop()
            result.add(index)
        if text == "(":
            paren += 1
        elif text == ")":
            paren -= 1
        elif text == "[":
            bracket += 1
        elif text == "]":
            bracket -= 1
        elif text == "{":
            brace += 1
        elif text == "}":
            brace -= 1
    return result


def transform_types_in_colon_positions(source: str) -> str:
    tokens = significant(source)
    parser = TypeParser(tokens)
    ternary = ternary_colons(tokens)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text != ":" or index in ternary or index + 1 >= len(tokens):
            continue
        parsed = parser.parse(index + 1)
        if parsed is None:
            continue
        rendered, end = parsed
        if end <= index + 1:
            continue
        old = source[tokens[index + 1].start : tokens[end - 1].end]
        if old.strip() != rendered:
            edits.append(
                Edit(tokens[index + 1].start, tokens[end - 1].end, rendered)
            )

    return apply_edits(source, edits)


def module_path_token_indexes(tokens: Sequence[Token]) -> set[int]:
    """Return indexes in import/export module-path clauses."""

    protected: set[int] = set()
    pairs = {"(": ")", "[": "]", "{": "}", "<": ">"}
    index = 0
    while index < len(tokens):
        declaration = tokens[index].text
        if declaration not in {"import", "export"}:
            index += 1
            continue

        stack: list[str] = []
        end = index + 1
        while end < len(tokens):
            text = tokens[end].text
            if not stack and text == ";":
                break
            if text in pairs:
                stack.append(pairs[text])
            elif stack and text == stack[-1]:
                stack.pop()
            end += 1
        if end >= len(tokens):
            index += 1
            continue

        stack.clear()
        top_level: list[int] = []
        for cursor in range(index + 1, end):
            text = tokens[cursor].text
            if not stack:
                top_level.append(cursor)
            if text in pairs:
                stack.append(pairs[text])
            elif stack and text == stack[-1]:
                stack.pop()

        path_start = index + 1
        path_end = end
        if declaration == "import":
            from_index = next(
                (
                    cursor
                    for cursor in top_level
                    if tokens[cursor].text == "from"
                ),
                None,
            )
            if from_index is not None:
                path_start = from_index + 1
            path_end = next(
                (
                    cursor
                    for cursor in top_level
                    if cursor >= path_start
                    and tokens[cursor].text == "hiding"
                ),
                end,
            )
        elif path_start < end and tokens[path_start].text == "{":
            for cursor in range(path_start + 1, end):
                if tokens[cursor].text != "@":
                    continue
                terminal = next(
                    (
                        candidate
                        for candidate in range(cursor + 1, end - 1)
                        if tokens[candidate].text == "."
                        and tokens[candidate + 1].text == "*"
                    ),
                    None,
                )
                if terminal is not None:
                    protected.update(range(cursor, terminal))
            index = end + 1
            continue
        else:
            path_end = next(
                (
                    cursor
                    for cursor in top_level
                    if cursor >= path_start
                    and (
                        tokens[cursor].text == "as"
                        or (
                            tokens[cursor].text == "."
                            and cursor + 1 < end
                            and tokens[cursor + 1].text in {"{", "*"}
                        )
                    )
                ),
                end,
            )

        protected.update(range(path_start, path_end))
        index = end + 1

    return protected


def transform_proxy_expressions(source: str) -> str:
    tokens = significant(source)
    parser = TypeParser(tokens)
    module_paths = module_path_token_indexes(tokens)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text != "@" or index + 1 >= len(tokens):
            continue
        if index in module_paths:
            continue
        parsed = parser.parse(index + 1)
        if parsed is None:
            continue
        rendered, end = parsed
        edits.append(
            Edit(
                token.start,
                tokens[end - 1].end,
                f"Proxy as Proxy<{rendered}>",
            )
        )
    return apply_edits(source, edits)


def protected_annotation_colons(tokens: Sequence[Token]) -> set[int]:
    protected = set(ternary_colons(tokens))

    # Function, constructor, fallback, and lambda parameter declarations.
    for index, token in enumerate(tokens):
        if token.text not in {"function", "constructor", "fallback", "lam"}:
            continue
        cursor = index + 1
        if token.text == "function":
            cursor += 1
            if cursor < len(tokens) and tokens[cursor].text == "<":
                close = matching_token(tokens, cursor)
                if close is not None:
                    cursor = close + 1
        if cursor < len(tokens) and tokens[cursor].text == "(":
            close = matching_token(tokens, cursor)
            if close is not None:
                for probe in range(cursor + 1, close):
                    if tokens[probe].text == ":":
                        protected.add(probe)

    # Named return items, including ``comptime name: Type``.  A returns clause
    # is entirely a type/declaration context, so none of its colons denote the
    # removed expression-annotation syntax.
    for index, token in enumerate(tokens):
        if (
            token.text != "returns"
            or index + 1 >= len(tokens)
            or tokens[index + 1].text != "("
        ):
            continue
        close = matching_token(tokens, index + 1)
        if close is not None:
            for probe in range(index + 2, close):
                if tokens[probe].text == ":":
                    protected.add(probe)

    # Let binding annotations.  The complete (possibly nested) binding pattern
    # precedes the colon.
    for index, token in enumerate(tokens):
        if token.text != "let":
            continue
        cursor = index + 1
        if cursor < len(tokens) and tokens[cursor].text == "comptime":
            cursor += 1
        if cursor >= len(tokens):
            continue
        if tokens[cursor].text == "(":
            close = matching_token(tokens, cursor)
            if close is None:
                continue
            annotation = close + 1
        elif tokens[cursor].kind == "ident" or tokens[cursor].text == "_":
            annotation = cursor + 1
        else:
            continue
        if (
            annotation < len(tokens)
            and tokens[annotation].text == ":"
        ):
            protected.add(annotation)

    # Trait/impl/function constraints.
    for index, token in enumerate(tokens):
        if token.text != "where":
            continue
        cursor = index + 1
        stack: list[str] = []
        while cursor < len(tokens):
            text = tokens[cursor].text
            if text in {"(", "[", "<"}:
                stack.append({"(": ")", "[": "]", "<": ">"}[text])
            elif stack and text == stack[-1]:
                stack.pop()
            elif not stack and text in {"{", ";"}:
                break
            elif text == ":":
                protected.add(cursor)
            cursor += 1

    # Name-first fields and deliberately incomplete name-first declarations.
    for index, token in enumerate(tokens):
        if token.text != ":" or index == 0:
            continue
        if tokens[index - 1].kind != "ident":
            continue
        before = tokens[index - 2].text if index >= 2 else None
        if before in {"{", "}", ";"}:
            protected.add(index)

    return protected


def transform_expression_annotations(source: str) -> str:
    tokens = significant(source)
    protected = protected_annotation_colons(tokens)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text == ":" and index not in protected:
            edits.append(Edit(token.start, token.end, " as "))
    return apply_edits(source, edits)


def normalize_as_spacing(source: str) -> str:
    tokens = significant(source)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text != "as" or index == 0 or index + 1 >= len(tokens):
            continue
        before = source[tokens[index - 1].end : token.start]
        after = source[token.end : tokens[index + 1].start]
        if before.strip() == "" and "\n" not in before and before != " ":
            edits.append(Edit(tokens[index - 1].end, token.start, " "))
        if after.strip() == "" and "\n" not in after and after != " ":
            edits.append(Edit(token.end, tokens[index + 1].start, " "))
    return apply_edits(source, edits)


def brace_header_kind(tokens: Sequence[Token], open_brace: int) -> str:
    boundary = declaration_boundary(tokens, open_brace)
    header = {token.text for token in tokens[boundary:open_brace]}
    for kind in (
        "assembly",
        "enum",
        "export",
        "import",
        "hiding",
        "struct",
        "contract",
        "trait",
        "impl",
        "function",
        "constructor",
        "fallback",
        "lam",
        "if",
        "for",
        "while",
        "unchecked",
        "case",
        "default",
        "match",
    ):
        if kind in header:
            return kind
    return "block"


def remove_block_statement_semicolons(source: str) -> str:
    """Drop the old optional semicolon after a braced statement."""

    tokens = significant(source)
    edits: list[Edit] = []
    for close, token in enumerate(tokens):
        if (
            token.text != "}"
            or close + 1 >= len(tokens)
            or tokens[close + 1].text != ";"
        ):
            continue
        open_brace = None
        depth = 0
        for probe in range(close, -1, -1):
            if tokens[probe].text == "}":
                depth += 1
            elif tokens[probe].text == "{":
                depth -= 1
                if depth == 0:
                    open_brace = probe
                    break
        if open_brace is None:
            continue
        if brace_header_kind(tokens, open_brace) in {
            "assembly",
            "match",
            "if",
            "for",
            "while",
            "unchecked",
        }:
            edits.append(
                Edit(tokens[close + 1].start, tokens[close + 1].end, "")
            )
    return apply_edits(source, edits)


def terminate_trailing_expression_statements(source: str) -> str:
    """Add the semicolon required by the new statement grammar.

    The old parser accepted a bare final expression in a body.  Limiting the
    edit to the token immediately before a non-declaration closing brace keeps
    enum members, contract members, comments, and Yul assembly untouched.
    """

    tokens = significant(source)
    edits: list[Edit] = []
    expression_end = {")", "]"}
    yul = assembly_token_indexes(tokens)

    for close, token in enumerate(tokens):
        if token.text != "}" or close in yul:
            continue
        open_brace = None
        depth = 0
        for probe in range(close, -1, -1):
            if tokens[probe].text == "}":
                depth += 1
            elif tokens[probe].text == "{":
                depth -= 1
                if depth == 0:
                    open_brace = probe
                    break
        if open_brace is None or close == open_brace + 1:
            continue
        kind = brace_header_kind(tokens, open_brace)
        if kind in {
            "assembly",
            "enum",
            "export",
            "import",
            "hiding",
            "struct",
            "contract",
            "trait",
            "impl",
            "match",
        }:
            continue
        previous = tokens[close - 1]
        if (
            previous.kind in {"ident", "number", "string"}
            or previous.text in expression_end
        ):
            top_level_tokens: list[Token] = []
            nested_braces = 0
            for probe in range(open_brace + 1, close):
                candidate = tokens[probe]
                if candidate.text == "{":
                    nested_braces += 1
                elif candidate.text == "}":
                    nested_braces -= 1
                elif nested_braces == 0:
                    top_level_tokens.append(candidate)
            is_single_function_expression = (
                kind in {"function", "fallback"}
                and ";" not in {candidate.text for candidate in top_level_tokens}
                and not {
                    "return",
                    "let",
                    "if",
                    "for",
                    "match",
                    "assembly",
                    "break",
                    "continue",
                }
                .intersection(candidate.text for candidate in top_level_tokens)
            )
            if is_single_function_expression and top_level_tokens:
                if (
                    len(top_level_tokens) == 2
                    and top_level_tokens[0].text == "("
                    and top_level_tokens[1].text == ")"
                ):
                    edits.append(
                        Edit(
                            top_level_tokens[0].start,
                            top_level_tokens[1].end,
                            "return;",
                        )
                    )
                else:
                    edits.append(
                        Edit(
                            top_level_tokens[0].start,
                            top_level_tokens[0].start,
                            "return ",
                        )
                    )
                    edits.append(Edit(previous.end, previous.end, ";"))
            else:
                edits.append(Edit(previous.end, previous.end, ";"))
    return apply_edits(source, edits)


def terminate_semicolonless_call_statements(source: str) -> str:
    """Terminate old call statements that are followed by another statement.

    The old grammar made the semicolon on an expression statement optional.
    ``terminate_trailing_expression_statements`` handles a final expression
    immediately before ``}``; this pass handles the remaining common form:
    an outermost call followed by the next statement on a later line.
    """

    tokens = significant(source)
    yul = assembly_token_indexes(tokens)
    edits: list[Edit] = []
    header_words = frozenset(
        {
            "case",
            "constructor",
            "enum",
            "fallback",
            "for",
            "function",
            "if",
            "impl",
            "match",
            "trait",
            "while",
        }
    )
    continuation_tokens = frozenset(
        {
            ")",
            "]",
            "}",
            ",",
            ".",
            ";",
            "+",
            "-",
            "*",
            "/",
            "%",
            "**",
            "<<",
            ">>",
            "<",
            ">",
            "<=",
            ">=",
            "==",
            "!=",
            "&&",
            "||",
            "&",
            "|",
            "^",
            "?",
            ":",
            "as",
        }
    )

    for close, token in enumerate(tokens[:-1]):
        if token.text != ")" or close in yul:
            continue
        following = tokens[close + 1]
        gap = source[token.end : following.start]
        if "\n" not in gap or following.text in continuation_tokens:
            continue

        boundary = close - 1
        paren_depth = 1
        while boundary >= 0:
            text = tokens[boundary].text
            if text == ")":
                paren_depth += 1
            elif text == "(":
                paren_depth -= 1
                if paren_depth == 0:
                    break
            boundary -= 1
        if boundary < 1:
            continue

        statement_start = declaration_boundary(tokens, boundary)
        statement_tokens = tokens[statement_start : close + 1]
        if not statement_tokens:
            continue
        if any(candidate.text in header_words for candidate in statement_tokens):
            continue
        if statement_tokens[0].kind != "ident":
            continue

        edits.append(Edit(token.end, token.end, ";"))

    return apply_edits(source, edits)


def _parse_expression_segment(
    tokens: Sequence[Token],
    start: int,
    stops: frozenset[str],
    records: dict[int, tuple[int, int, int]],
) -> int:
    index = start
    while index < len(tokens):
        text = tokens[index].text
        if text in stops:
            return index
        if text == "if":
            parsed = _parse_if_expression(tokens, index, stops, records)
            if parsed is None:
                index += 1
            else:
                index = parsed
            continue
        if text in {"(", "["}:
            close = matching_token(tokens, index)
            if close is None:
                return index
            _parse_expression_segment(
                tokens,
                index + 1,
                frozenset({tokens[close].text}),
                records,
            )
            index = close + 1
            continue
        index += 1
    return index


def _parse_if_expression(
    tokens: Sequence[Token],
    index: int,
    outer_stops: frozenset[str],
    records: dict[int, tuple[int, int, int]],
) -> int | None:
    then = _parse_expression_segment(
        tokens, index + 1, frozenset({"then"}), records
    )
    if then >= len(tokens) or tokens[then].text != "then":
        return None
    otherwise = _parse_expression_segment(
        tokens, then + 1, frozenset({"else"}), records
    )
    if otherwise >= len(tokens) or tokens[otherwise].text != "else":
        return None
    end = _parse_expression_segment(tokens, otherwise + 1, outer_stops, records)
    records[index] = (then, otherwise, end)
    return end


def transform_if_expressions(source: str) -> str:
    tokens = significant(source)
    records: dict[int, tuple[int, int, int]] = {}
    consumed_until = 0
    for index, token in enumerate(tokens):
        if index < consumed_until or token.text != "if":
            continue
        end = _parse_if_expression(
            tokens,
            index,
            frozenset({";", "}", ",", ")"}),
            records,
        )
        if end is not None:
            consumed_until = max(consumed_until, end)

    edits: list[Edit] = []
    for start, (then, otherwise, end) in records.items():
        edits.append(Edit(tokens[start].start, tokens[start].end, "("))
        edits.append(Edit(tokens[then].start, tokens[then].end, "?"))
        edits.append(Edit(tokens[otherwise].start, tokens[otherwise].end, ":"))
        end_offset = tokens[end].start if end < len(tokens) else len(source)
        edits.append(Edit(end_offset, end_offset, ")"))
    return apply_edits(source, edits)


def parenthesize_control_conditions(source: str) -> str:
    """Parenthesize old ``if cond`` / ``while cond`` statements."""

    tokens = significant(source)
    yul = assembly_token_indexes(tokens)
    edits: list[Edit] = []
    for index, token in enumerate(tokens):
        if token.text not in {"if", "while"} or index in yul:
            continue
        condition_start = index + 1
        if (
            condition_start >= len(tokens)
            or tokens[condition_start].text == "("
        ):
            continue
        stack: list[str] = []
        open_body = None
        for cursor in range(condition_start, len(tokens)):
            text = tokens[cursor].text
            if text in {"(", "["}:
                stack.append({"(": ")", "[": "]"}[text])
            elif stack and text == stack[-1]:
                stack.pop()
            elif not stack and text == "{":
                open_body = cursor
                break
            elif not stack and text in {";", "}"}:
                break
        if open_body is None:
            continue
        edits.append(
            Edit(
                tokens[condition_start].start,
                tokens[condition_start].start,
                "(",
            )
        )
        edits.append(
            Edit(tokens[open_body].start, tokens[open_body].start, ") ")
        )
    return apply_edits(source, edits)


def transform_legacy_user_operators(source: str) -> str:
    """Lower the removed infix declaration form to ordinary helper calls.

    The tracked corpus has one such declaration and one simple use.  Keep this
    deliberately narrow rather than retaining an unspecified operator grammar.
    """

    tokens = significant(source)
    declarations: list[tuple[int, int, tuple[str, ...], str]] = []
    for index, token in enumerate(tokens):
        if token.text not in {"infixl", "infixr", "infix"}:
            continue
        end = index + 1
        while end < len(tokens) and tokens[end].text != ";":
            end += 1
        if end >= len(tokens):
            continue
        open_paren = next(
            (
                cursor
                for cursor in range(index + 1, end)
                if tokens[cursor].text == "("
            ),
            None,
        )
        arrow = next(
            (
                cursor
                for cursor in range(index + 1, end)
                if tokens[cursor].text == "=>"
            ),
            None,
        )
        if open_paren is None or arrow is None:
            continue
        close_paren = matching_token(tokens, open_paren)
        if (
            close_paren is None
            or close_paren >= arrow
            or arrow + 1 >= end
            or tokens[arrow + 1].kind != "ident"
        ):
            continue
        operator = tuple(
            candidate.text
            for candidate in tokens[open_paren + 1 : close_paren]
        )
        if not operator:
            continue
        declarations.append(
            (index, end, operator, tokens[arrow + 1].text)
        )

    if not declarations:
        return source

    edits: list[Edit] = []
    declaration_indexes: set[int] = set()
    for start, end, _, _ in declarations:
        declaration_indexes.update(range(start, end + 1))
        line_start = source.rfind("\n", 0, tokens[start].start) + 1
        line_end = source.find("\n", tokens[end].end)
        if line_end < 0:
            line_end = len(source)
        else:
            line_end += 1
        edits.append(Edit(line_start, line_end, ""))

    for _, _, operator, helper in declarations:
        width = len(operator)
        for index in range(1, len(tokens) - width):
            if index in declaration_indexes:
                continue
            if tuple(
                candidate.text
                for candidate in tokens[index : index + width]
            ) != operator:
                continue
            left = tokens[index - 1]
            right_index = index + width
            right = tokens[right_index]
            if (
                left.kind not in {"ident", "number"}
                or right.kind not in {"ident", "number"}
            ):
                continue
            edits.append(
                Edit(
                    left.start,
                    right.end,
                    f"{helper}({left.text}, {right.text})",
                )
            )
    return apply_edits(source, edits)


def remove_yul_semicolons(source: str) -> str:
    """Yul statements are whitespace-delimited; ``;`` is not valid there."""

    tokens = significant(source)
    yul = assembly_token_indexes(tokens)
    edits = [
        Edit(token.start, token.end, "")
        for index, token in enumerate(tokens)
        if index in yul and token.text == ";"
    ]
    return apply_edits(source, edits)


def migrate_source(source: str) -> str:
    passes = (
        transform_imports,
        transform_pragmas,
        transform_data_declarations,
        transform_type_declarations,
        transform_traits_and_impls,
        transform_functions,
        transform_let_comptime,
        transform_matches,
        transform_if_expressions,
        parenthesize_control_conditions,
        transform_legacy_user_operators,
        transform_types_in_colon_positions,
        transform_proxy_expressions,
        transform_expression_annotations,
        normalize_as_spacing,
        remove_yul_semicolons,
        remove_block_statement_semicolons,
        terminate_semicolonless_call_statements,
        terminate_trailing_expression_statements,
    )
    migrated = source
    for migration_pass in passes:
        migrated = migration_pass(migrated)
    return migrated


def apply_file_fixups(relative: pathlib.Path, source: str) -> str:
    fixed = source
    for old, new in FILE_FIXUPS.get(relative, ()):
        fixed = fixed.replace(old, new)
    return fixed


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
        for path in (REPO_ROOT / root_name).rglob("*.solc")
        if path.is_file() and not path.is_symlink()
    )


def tracked_core_sources() -> list[pathlib.Path]:
    tracked_solc: list[pathlib.Path]
    if not (REPO_ROOT / ".git").exists():
        tracked_solc = packaged_solc_sources()
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
                "*.solc",
            ],
            cwd=REPO_ROOT,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        tracked_solc = [
            pathlib.Path(raw_path.decode("utf-8"))
            for entry in process.stdout.split(b"\0")
            if entry
            for metadata, raw_path in [entry.split(b"\t", 1)]
            if metadata.split(maxsplit=1)[0] != b"120000"
        ]
    paths = [*tracked_solc, *(pathlib.Path(path) for path in CORE_SOL_FILES)]
    return sorted(dict.fromkeys(paths))


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
                    f"HEAD:{relative.as_posix()}",
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
