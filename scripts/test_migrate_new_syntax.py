#!/usr/bin/env python3
"""Focused regression tests for the source-corpus migration."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


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

    def test_nested_block_comments_are_stable(self) -> None:
        self.assert_stable(
            "/* outer /* inner */ type Word = word; */\n"
            "function ok() {}\n"
        )

    def test_function_type_visibility_is_stable(self) -> None:
        for visibility in ("", " internal", " external"):
            with self.subTest(visibility=visibility):
                self.assert_stable(
                    "function apply("
                    f"callback: function(word){visibility} returns (bool)"
                    ") {}\n"
                )

    def test_explicit_unit_returns_are_stable(self) -> None:
        cases = (
            "function unitValue() returns (()) { return (); }\n",
            (
                "function named() returns (result: word) "
                "{ result = 1; return (); }\n"
            ),
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)

    def test_yul_meta_payloads_are_stable(self) -> None:
        cases = (
            "function f() { assembly { let x := `foo;bar` } }\n",
            "function f() { assembly { let x := ${foo;bar} } }\n",
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)

    def test_transparent_aliases_are_stable(self) -> None:
        cases = (
            "alias Word = uint256;\n",
            "alias Map<key, value> = pair<key, value>;\n",
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)

    def test_nominal_user_defined_value_type_syntax_is_stable(self) -> None:
        self.assert_stable("type Wad is uint256;\n")

    def test_external_module_imports_are_stable(self) -> None:
        cases = (
            "import @ext.foo.bar;\n",
            "import * as Foo from @ext.foo.bar;\n",
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)

    def test_external_selective_import_is_stable(self) -> None:
        self.assert_stable(
            "import {foo, bar as baz} from @ext.foo.bar;\n"
        )

    def test_external_glob_import_with_hiding_is_stable(self) -> None:
        self.assert_stable(
            "import {*} from @ext.foo.bar hiding {bar};\n"
        )

    def test_external_exports_are_stable(self) -> None:
        cases = (
            "export @ext.foo.bar;\n",
            "export @ext.foo.bar as Foo;\n",
            "export @ext.foo.bar.{foo};\n",
            "export @ext.foo.bar.*;\n",
            "export {@ext.foo.bar.*};\n",
        )
        for source in cases:
            with self.subTest(source=source):
                self.assert_stable(source)

    def test_tracked_symlink_aliases_are_not_counted_as_sources(self) -> None:
        sources = migration.tracked_core_sources()
        self.assertFalse(
            any((migration.REPO_ROOT / source).is_symlink() for source in sources)
        )

    def test_absolute_symlink_alias_cannot_bypass_source_allow_list(self) -> None:
        alias = migration.REPO_ROOT / "test/imports/mirror/api.solc"
        self.assertTrue(alias.is_symlink())
        for argument in (str(alias.relative_to(migration.REPO_ROOT)), str(alias)):
            with self.subTest(argument=argument):
                with self.assertRaisesRegex(ValueError, "symlink source"):
                    migration.eligible_paths([argument])

    def test_regular_index_entries_cannot_write_through_worktree_symlinks(
        self,
    ) -> None:
        legacy_source = "type Word = word;\n"
        for layout in ("source", "parent"):
            with self.subTest(layout=layout), tempfile.TemporaryDirectory() as directory:
                temporary_root = pathlib.Path(directory)
                root = temporary_root / "repo"
                root.mkdir()
                subprocess.run(
                    ["git", "init", "--quiet"],
                    cwd=root,
                    check=True,
                )

                relative = pathlib.Path("src/nested/victim.solc")
                tracked = root / relative
                tracked.parent.mkdir(parents=True)
                tracked.write_text(legacy_source)
                subprocess.run(
                    ["git", "add", "--", relative.as_posix()],
                    cwd=root,
                    check=True,
                )
                subprocess.run(
                    [
                        "git",
                        "-c",
                        "user.name=Migration Test",
                        "-c",
                        "user.email=migration-test@example.invalid",
                        "commit",
                        "--quiet",
                        "-m",
                        "fixture",
                    ],
                    cwd=root,
                    check=True,
                )
                index_entry = subprocess.run(
                    ["git", "ls-files", "-s", "--", relative.as_posix()],
                    cwd=root,
                    check=True,
                    stdout=subprocess.PIPE,
                    text=True,
                ).stdout
                self.assertTrue(index_entry.startswith("100644 "))

                if layout == "source":
                    target = temporary_root / "outside.solc"
                    target.write_text(legacy_source)
                    tracked.unlink()
                    tracked.symlink_to(target)
                else:
                    linked_parent = tracked.parent
                    outside_parent = temporary_root / "outside-parent"
                    linked_parent.rename(outside_parent)
                    linked_parent.symlink_to(
                        outside_parent,
                        target_is_directory=True,
                    )
                    target = outside_parent / tracked.name

                with (
                    mock.patch.object(migration, "REPO_ROOT", root),
                    mock.patch.object(migration, "CORE_SOL_FILES", ()),
                ):
                    for arguments in (
                        [],
                        [relative.as_posix()],
                        [str(root / relative)],
                    ):
                        with self.subTest(arguments=arguments):
                            with self.assertRaisesRegex(
                                ValueError,
                                "symlink source",
                            ):
                                migration.eligible_paths(arguments)

                    for arguments in (
                        ["--write"],
                        ["--write", relative.as_posix()],
                        ["--write", str(root / relative)],
                        ["--write", "--from-head", relative.as_posix()],
                    ):
                        with self.subTest(cli_arguments=arguments):
                            with (
                                contextlib.redirect_stderr(io.StringIO()),
                                self.assertRaises(SystemExit) as raised,
                            ):
                                migration.main(arguments)
                            self.assertEqual(raised.exception.code, 2)
                            self.assertEqual(target.read_text(), legacy_source)

    def test_git_failure_does_not_expand_write_scope_to_untracked_files(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            (root / ".git").mkdir()
            failures = (
                subprocess.CalledProcessError(128, ["git", "ls-files"]),
                FileNotFoundError("git"),
            )
            for failure in failures:
                with self.subTest(failure=type(failure).__name__):
                    with (
                        mock.patch.object(migration, "REPO_ROOT", root),
                        mock.patch.object(
                            migration.subprocess,
                            "run",
                            side_effect=failure,
                        ),
                        mock.patch.object(
                            migration,
                            "packaged_solc_sources",
                        ) as fallback,
                    ):
                        with self.assertRaises(type(failure)):
                            migration.tracked_core_sources()
                        fallback.assert_not_called()

    def test_from_head_uses_safe_directory_and_never_writes_after_git_failure(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            relative = pathlib.Path("src/example.solc")
            source = root / relative
            source.parent.mkdir(parents=True)
            source.write_text("function current() {}\n")
            failure = subprocess.CalledProcessError(
                128,
                ["git", "show", f"HEAD:{relative.as_posix()}"],
                stderr="fatal: detected dubious ownership in repository",
            )

            def reject_git_show(
                command: list[str],
                **kwargs: object,
            ) -> subprocess.CompletedProcess[str]:
                self.assertEqual(
                    command,
                    [
                        "git",
                        "-c",
                        f"safe.directory={root}",
                        "show",
                        f"HEAD:{relative.as_posix()}",
                    ],
                )
                self.assertEqual(kwargs["cwd"], root)
                raise failure

            with (
                mock.patch.object(migration, "REPO_ROOT", root),
                mock.patch.object(
                    migration,
                    "eligible_paths",
                    return_value=[relative],
                ),
                mock.patch.object(
                    migration.subprocess,
                    "run",
                    side_effect=reject_git_show,
                ),
                mock.patch.object(migration, "write_worktree_source") as write,
            ):
                with self.assertRaises(subprocess.CalledProcessError) as raised:
                    migration.main(
                        ["--write", "--from-head", relative.as_posix()]
                    )
                self.assertIs(raised.exception, failure)
                write.assert_not_called()

    def test_packaged_source_fallback_is_scoped_to_core_roots(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            for relative in ("src/a.solc", "std/b.solc", "test/c.solc"):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("function current() {}\n")
            (root / "scratch.solc").write_text("legacy root scratch\n")
            (root / "poc").mkdir()
            (root / "poc/experiment.solc").write_text("legacy experiment\n")
            (root / "test/link.solc").symlink_to(root / "src/a.solc")

            with mock.patch.object(migration, "REPO_ROOT", root):
                self.assertEqual(
                    migration.packaged_solc_sources(),
                    [
                        pathlib.Path("src/a.solc"),
                        pathlib.Path("std/b.solc"),
                        pathlib.Path("test/c.solc"),
                    ],
                )


class LegacyMigrationTests(unittest.TestCase):
    def test_transparent_type_declarations_become_aliases(self) -> None:
        cases = (
            ("type Word = word;\n", "alias Word = word;\n"),
            (
                "type Pair(a, b) = pair(a, b);\n",
                "alias Pair<a, b> = pair<a, b>;\n",
            ),
        )
        for source, expected in cases:
            with self.subTest(source=source):
                self.assertEqual(migration.migrate_source(source), expected)

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

    def test_external_selective_import_migrates(self) -> None:
        source = "import @ext.foo.bar.{foo, bar as baz};\n"
        expected = "import {foo, bar as baz} from @ext.foo.bar;\n"
        self.assertEqual(migration.migrate_source(source), expected)

    def test_proxy_expression_outside_module_path_still_migrates(self) -> None:
        source = "function f() { return @Foo.bar; }\n"
        expected = (
            "function f() { return Proxy as Proxy<Foo.bar>; }\n"
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
