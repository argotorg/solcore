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


class NewSyntaxMigrationTests(unittest.TestCase):
    def assert_migration(self, source: str, expected: str) -> None:
        result = migration.migrate_source(source)
        self.assertEqual(result, expected)
        self.assertEqual(migration.migrate_source(result), result)

    def test_types_and_aliases(self) -> None:
        self.assert_migration(
            "alias Ref<a> = a storage;\nfunction f(xs: word[] memory) returns (word) {}\n",
            "type Ref(a) = storage<a>;\nfunction f(xs: memory<array<word>>) returns (word) {}\n",
        )

    def test_comptime_parameter_local_and_return(self) -> None:
        self.assert_migration(
            "function f(comptime x: word) returns (comptime word) { let comptime y: word = x; return y; }",
            "function f(comptime x: word) returns (comptime<word>) { let y: comptime<word> = x; return y; }",
        )

    def test_lambda_result_and_proxy(self) -> None:
        self.assert_migration(
            "let f = lam (x: word) returns (word) { return x; }; let p = Proxy as Proxy<option<word>>;",
            "let f = lam (x: word) -> word { return x; }; let p = @option<word>;",
        )

    def test_annotations_retain_expected_type(self) -> None:
        self.assert_migration(
            "function f() { return load(storage(2) as word storage); }",
            "function f() { let syntaxValue1: storage<word> = storage(2); return load(syntaxValue1); }",
        )

    def test_lazy_operands_and_previous_calls_keep_evaluation_order(self) -> None:
        for expression in ("ready && (read() as bool)", "call(first(), read() as word)"):
            with self.subTest(expression=expression):
                migrated = migration.migrate_source("function f() { return " + expression + "; }")
                self.assertNotIn("let syntaxValue1", migrated)
                self.assertIn("lam (syntaxValue:", migrated)
                self.assertEqual(migration.migrate_source(migrated), migrated)

    def test_nested_annotations_become_ordered_typed_bindings(self) -> None:
        migrated = migration.migrate_source("function f<a, b>() { return (load(item as a) as b); }")
        self.assertNotIn("lam (syntaxValue:", migrated)
        self.assertLess(migrated.index("= item;"), migrated.index("= load("))
        self.assertEqual(migration.migrate_source(migrated), migrated)

    def test_new_prefix_types_after_where_are_stable(self) -> None:
        source = "impl<a> Store<storage<a>> where storage<a>: Store<a> {}"
        self.assert_migration(source, source)

    def test_import_aliases_comments_and_assembly_are_preserved(self) -> None:
        source = ("import {foo as bar} from util;\n"
                  "// let comptime x: word = 1;\n"
                  "/* outer /* nested */ alias X = word; */\n"
                  'function f() { assembly { let _x := \"returns (comptime word)\" } }')
        self.assert_migration(source, source)

    def test_pragma_names(self) -> None:
        self.assert_migration("pragma solcore noBoundVariableCondition Store;", "pragma  no-bounded-variable-condition Store;")

    def test_tracked_symlink_aliases_are_not_counted_as_sources(self) -> None:
        sources = migration.tracked_core_sources()
        self.assertFalse(
            any((migration.REPO_ROOT / source).is_symlink() for source in sources)
        )

    def test_absolute_symlink_alias_cannot_bypass_source_allow_list(self) -> None:
        alias = migration.REPO_ROOT / "test/imports/mirror/api.sol"
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

                relative = pathlib.Path("src/nested/victim.sol")
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
                    target = temporary_root / "outside.sol"
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
            relative = pathlib.Path("src/example.sol")
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
                    migration,
                    "tracked_core_source_origins",
                    return_value={relative: relative},
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
            for relative in ("src/a.sol", "std/b.sol", "test/c.sol"):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("function current() {}\n")
            (root / "scratch.sol").write_text("legacy root scratch\n")
            (root / "poc").mkdir()
            (root / "poc/experiment.sol").write_text("legacy experiment\n")
            (root / "test/link.sol").symlink_to(root / "src/a.sol")
            classic = root / "test/examples/dispatch/fib.classic.sol"
            classic.parent.mkdir(parents=True)
            classic.write_text("pragma solidity ^0.8.0;\n")

            with mock.patch.object(migration, "REPO_ROOT", root):
                self.assertEqual(
                    migration.packaged_solc_sources(),
                    [
                        pathlib.Path("src/a.sol"),
                        pathlib.Path("std/b.sol"),
                        pathlib.Path("test/c.sol"),
                    ],
                )

    def test_unstaged_extension_rename_keeps_core_head_origin(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            original = pathlib.Path("test/examples/dispatch/fib.solc")
            renamed = original.with_suffix(".sol")
            classic = original.with_name("fib.classic.sol")
            (root / original).parent.mkdir(parents=True)
            (root / original).write_text("alias Word = word;\n")
            (root / renamed).write_text("pragma solidity ^0.8.0;\n")
            subprocess.run(["git", "init", "--quiet"], cwd=root, check=True)
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(
                ["git", "-c", "user.name=Migration Test", "-c",
                 "user.email=migration-test@example.invalid", "commit", "--quiet", "-m", "fixture"],
                cwd=root, check=True,
            )
            with mock.patch.object(migration, "REPO_ROOT", root), mock.patch.object(migration, "CORE_SOL_FILES", ()):
                # Before the extension change, only the Core .solc is eligible.
                self.assertEqual(migration.tracked_core_sources(), [original])
                (root / renamed).rename(root / classic)
                (root / original).rename(root / renamed)
                self.assertEqual(migration.tracked_core_source_origins(), {renamed: original})
                with contextlib.redirect_stdout(io.StringIO()):
                    self.assertEqual(migration.main(["--write", "--from-head", str(renamed)]), 0)
                self.assertEqual((root / renamed).read_text(), "type Word = word;\n")
                self.assertEqual((root / classic).read_text(), "pragma solidity ^0.8.0;\n")
                with contextlib.redirect_stdout(io.StringIO()):
                    self.assertEqual(migration.main(["--check"]), 0)

    def test_unstaged_extension_rename_cannot_follow_replacement_symlink(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory) / "repo"
            root.mkdir()
            original = pathlib.Path("src/example.solc")
            renamed = original.with_suffix(".sol")
            (root / original).parent.mkdir()
            (root / original).write_text("alias Word = word;\n")
            subprocess.run(["git", "init", "--quiet"], cwd=root, check=True)
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            (root / original).unlink()
            outside = root.parent / "outside.sol"
            outside.write_text("alias Outside = word;\n")
            (root / renamed).symlink_to(outside)
            with mock.patch.object(migration, "REPO_ROOT", root), mock.patch.object(migration, "CORE_SOL_FILES", ()):
                with self.assertRaisesRegex(ValueError, "symlink source"):
                    migration.eligible_paths([])
                with self.assertRaisesRegex(ValueError, "symlink source"):
                    migration.eligible_paths([str(renamed)])
            self.assertEqual(outside.read_text(), "alias Outside = word;\n")


if __name__ == "__main__":
    unittest.main()
