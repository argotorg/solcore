pragma no-patterson-condition ABIAttribs, ABIEncode;
pragma no-bounded-variable-condition ABIAttribs, ABIEncode;

import std.{*};
import std.Generic.{*};

// Minimal reproducer for the "imported-default-instance-stub mis-tagged" bug.
//
// std/Generic.solc exports:
//   forall a rep . a:Generic(rep), rep:ABIAttribs, rep:ABIEncode =>
//   default instance a : ABIEncode { function encodeInto ... }
//
// This file redefines the exact same default instance locally.
// The instance head (True, "ABIEncode", [], TyVar "a") is shared.
//
// Bug path:
//   1. filterImportedInstanceConflicts uses topDeclClassNames, which returns []
//      because this file defines no class -- only instances.  The imported stub
//      is NOT filtered.
//   2. moduleInferenceDeclSegmentByKey maps the shared key to ModuleLocalDecl
//      (the local definition arrives first in the ordered list).
//   3. retagModuleInferenceDecls retags the imported stub with the same key,
//      giving it ModuleLocalDecl / CheckTopDeclBody mode.
//   4. tcTopDeclWithVisibility calls tcTopDecl' on the stub (funs = []).
//   5. tcInstance' -> checkCompleteInstDef -> "Incomplete definition for ABIEncode".

forall a rep . a:Generic(rep), rep:ABIAttribs, rep:ABIEncode =>
default instance a : ABIEncode {
    function encodeInto(x : a, basePtr : word, offset : word, tail : word) -> word {
        return ABIEncode.encodeInto(Generic.from(x), basePtr, offset, tail);
    }
}
