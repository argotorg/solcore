#!/usr/bin/env python3
"""Focused regression tests for the source-corpus migration."""

from __future__ import annotations

import importlib.util
import pathlib
import sys
import unittest


SCRIPT = pathlib.Path(__file__).with_name("migrate_new_syntax.py")
SPEC = importlib.util.spec_from_file_location("migrate_new_syntax", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
migration = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = migration
SPEC.loader.exec_module(migration)


class NewSyntaxStabilityTests(unittest.TestCase):
    def assert_stable(self, source: str) -> None:
        self.assertEqual(migration.migrate_source(source), source)

    def test_named_and_comptime_returns_are_stable(self) -> None:
        self.assert_stable(
            "function namedPair() returns "
            "(left: uint256, comptime right: bool) "
            "{ return (1, true); }\n"
        )

    def test_recursive_tuple_binding_is_stable(self) -> None:
        for comptime in ("", "comptime "):
            with self.subTest(comptime=comptime):
                self.assert_stable(
                    "function unpack() { let "
                    f"{comptime}(a, (b, c)): "
                    "(uint256, (bool, word)) = readResult(); }\n"
                )

    def test_data_identifiers_are_stable(self) -> None:
        cases = (
            "function f(data: word) returns (word) { return data; }\n",
            "struct S { data: word; }\n",
            "contract C { data: word; }\n",
            "function f() { let data: word = 1; data = data + 1; }\n",
            "function f(x: S) { x.data; }\n",
            "function f() { assembly { let data := calldataload(0) } }\n",
            (
                "// data Fake = Fake;\n"
                'function f() returns (string) '
                '{ return "data Fake = Fake;"; }\n'
            ),
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)


class LegacyMigrationTests(unittest.TestCase):
    def test_expression_annotation_still_becomes_conversion(self) -> None:
        source = (
            "function convert(x: word) returns (word) "
            "{ return x : word; }\n"
        )
        expected = (
            "function convert(x: word) returns (word) "
            "{ return x as word; }\n"
        )
        self.assertEqual(migration.migrate_source(source), expected)

    def test_top_level_and_contract_data_declarations_migrate(self) -> None:
        source = (
            "data Option(a) = None | Some(a);\n"
            "contract C {\n"
            "  data Pair(a, b) = Pair(a, b);\n"
            "}\n"
        )
        expected = (
            "enum Option<a> { None, Some(a) }\n"
            "contract C {\n"
            "  enum Pair<a, b> { Pair(a, b) }\n"
            "}\n"
        )
        self.assertEqual(migration.migrate_source(source), expected)

    def test_sum_parameter_fixup_is_reproducible(self) -> None:
        generated = (
            "function sum (p1 : (T1, T2), p2 (T1, T2)) "
            "returns ((T1, T2)) {"
        )
        expected = (
            "function sum (p1 : (T1, T2), p2 : (T1, T2)) "
            "returns ((T1, T2)) {"
        )
        self.assertEqual(
            migration.apply_file_fixups(
                pathlib.Path("blog-post/sum.sol"), generated
            ),
            expected,
        )


if __name__ == "__main__":
    unittest.main()
