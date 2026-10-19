#!/usr/bin/env python3
# Generates std/signed.sol (int8..int256) and std/unsigned.sol (uint8..uint128),
# mirroring the operations std provides for uint256 but with the right semantics:
#   * signed:   Div/Mod via sdiv/smod, Ord via sgt, narrow widths kept in range
#               with signextend; every value is a 2's-complement word.
#   * unsigned: Div/Mod via div/mod, Ord via unsigned gt, narrow widths kept in
#               range by masking with 2^n - 1.
# uint256 already lives in std/std.sol, so it is NOT re-generated here.

SIGNED = [("int8", 8), ("int16", 16), ("int32", 32), ("int64", 64), ("int128", 128), ("int256", 256)]
UNSIGNED = [("uint8", 8), ("uint16", 16), ("uint32", 32), ("uint64", 64), ("uint128", 128)]


def reduce_expr(inner, bits, signed):
    """Bring a full-word result back into the type's n-bit range."""
    if bits == 256:
        return inner
    if signed:
        return f"signextend({bits // 8 - 1}, {inner})"
    mask = (1 << bits) - 1
    return f"BitAnd.band({inner}, 0x{mask:x})"


def wrap(name, inner, bits, signed):
    return f"Typedef.abs({reduce_expr(inner, bits, signed)})"


def rep(v):
    return f"Typedef.rep({v})"


def gen_type(name, bits, signed):
    L = []
    a, b = "x", "y"
    rx, ry = rep(a), rep(b)

    L.append(f"// --- {name} ({bits}-bit {'signed' if signed else 'unsigned'}) ---")
    L.append(f"enum {name} {{ {name}(word) }}")
    L.append(f"impl Typedef<{name}, word> {{")
    L.append(f"  function abs(w : word) returns ({name}) {{ return {name}(w); }}")
    L.append(f"  function rep(x : {name}) returns (word) {{ match (x) {{ case {name}(w) {{ return w; }} }} }}")
    L.append("}")

    def binop(trait, method, expr):
        L.append(f"impl {trait}<{name}> {{")
        L.append(f"  function {method}(x : {name}, y : {name}) returns ({name}) {{ return {wrap(name, expr, bits, signed)}; }}")
        L.append("}")

    binop("Add", "add", f"Add.add({rx}, {ry})")
    binop("Sub", "sub", f"Sub.sub({rx}, {ry})")
    binop("Mul", "mul", f"Mul.mul({rx}, {ry})")
    if signed:
        binop("Div", "div", f"sdiv({rx}, {ry})")
        binop("Mod", "mod", f"smod({rx}, {ry})")
    else:
        binop("Div", "div", f"Div.div({rx}, {ry})")
        binop("Mod", "mod", f"Mod.mod({rx}, {ry})")
    binop("BitAnd", "band", f"BitAnd.band({rx}, {ry})")
    binop("BitOr", "bor", f"BitOr.bor({rx}, {ry})")
    binop("BitXor", "bxor", f"BitXor.bxor({rx}, {ry})")

    # BitNot is unary.
    L.append(f"impl BitNot<{name}> {{")
    L.append(f"  function bnot(x : {name}) returns ({name}) {{ return {wrap(name, f'BitNot.bnot({rx})', bits, signed)}; }}")
    L.append("}")

    # Eq (same for both).
    L.append(f"impl Eq<{name}> {{")
    L.append(f"  function eq(x : {name}, y : {name}) returns (bool) {{ return Eq.eq({rx}, {ry}); }}")
    L.append("}")

    # Ord: signed comparison vs unsigned comparison.
    gt = f"tobool(sgt({rx}, {ry}))" if signed else f"Ord.gt({rx}, {ry})"
    L.append(f"impl Ord<{name}> {{")
    L.append(f"  function gt(x : {name}, y : {name}) returns (bool) {{ return {gt}; }}")
    L.append("}")

    # Bounded.
    if signed:
        mx = (1 << (bits - 1)) - 1
        mn = (1 << 256) - (1 << (bits - 1))  # two's-complement word for -2^(bits-1)
    else:
        mx = (1 << bits) - 1
        mn = 0
    L.append(f"impl Bounded<{name}> {{")
    L.append(f"  function minVal() returns ({name}) {{ return {name}(0x{mn:x}); }}")
    L.append(f"  function maxVal() returns ({name}) {{ return {name}(0x{mx:x}); }}")
    L.append("}")

    # Int (integer literal coercion). Mirrors uint256: no runtime reduce, so the
    # comptime literal folds through wordFromInteger (an in-range literal is
    # already correct -- a negative one is its own sign-extended two's complement).
    L.append(f"impl Int<{name}> {{")
    L.append(f"  function fromInteger(x : integer) returns ({name}) {{ return {name}(wordFromInteger(x)); }}")
    L.append("}")

    # SigString: the ABI signature name (matches the canonical Solidity type
    # name). Without this the newtype falls back to its Generic rep and the
    # method selector would not match `name(int8)` etc.
    L.append(f"impl SigString<{name}> {{")
    L.append(f"  function sigStr(x : Proxy<{name}>) returns (string) {{ return \"{name}\"; }}")
    L.append("}")

    # --- ABI (static 32-byte word) + storage (one slot), mirroring uint256 ---
    L.append(f"impl ABIAttribs<{name}> {{")
    L.append(f"  function headSize(ty : Proxy<{name}>) returns (word) {{ return 32; }}")
    L.append(f"  function isStatic(ty : Proxy<{name}>) returns (bool) {{ return true; }}")
    L.append("}")
    L.append(f"impl ABIEncode<{name}> {{")
    L.append(f"  function encodeInto(x : {name}, basePtr : word, offset : word, tail : word) returns (word) {{")
    L.append(f"    mstore(basePtr + offset, Typedef.rep(x));")
    L.append(f"    return tail;")
    L.append("  }")
    L.append("}")
    # Decode: read one word, then bring it back into the type's range.
    dec = reduce_expr("WordReader.read(WordReader.advance(ptr, currentHeadOffset))", bits, signed)
    L.append(f"impl<reader> ABIDecode<ABIDecoder<{name}, reader>, {name}> where reader: WordReader {{")
    L.append(f"  function decode(ptr : ABIDecoder<{name}, reader>, currentHeadOffset : word) returns ({name}) {{")
    L.append(f"    return Typedef.abs({dec});")
    L.append("  }")
    L.append("}")
    L.append(f"impl StorageSize<{name}> {{")
    L.append(f"  function size(x : Proxy<{name}>) returns (word) {{ return 1; }}")
    L.append("}")
    L.append(f"impl StorageType<{name}> {{")
    L.append(f"  function load(ptr : word) returns ({name}) {{ return {name}(StorageType.load(ptr)); }}")
    L.append(f"  function store(ptr : word, value : {name}) returns (()) {{ StorageType.store(ptr, Typedef.rep(value)); }}")
    L.append("}")
    # CanStore lets a value be a contract field / mapping value.
    L.append(f"impl CanStore<storage<{name}>, {name}> {{")
    L.append(f"  function store(l : storage<{name}>, r : {name}) returns (()) {{ StorageType.store(Typedef.rep(l), r); }}")
    L.append(f"  function load(l : storage<{name}>) returns ({name}) {{ return StorageType.load(Typedef.rep(l)); }}")
    L.append("}")
    L.append(f"impl StorageCopy<{name}> {{")
    L.append(f"  function copySlot(dst : storage<{name}>, src : storage<{name}>) returns (()) {{")
    L.append(f"    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));")
    L.append("  }")
    L.append("}")

    L.append("")
    return "\n".join(L)


def gen_module(path, title, types, signed):
    out = []
    out.append(f"// {title}")
    out.append("// GENERATED by scripts/gen-std-int.py -- do not edit by hand.")
    out.append("//")
    out.append("// Each type wraps a two's-complement `word`; narrow widths are kept in range")
    out.append("// (" + ("signextend" if signed else "mask 2^n-1") + ") after every value-producing operation.")
    out.append("import * from std;")
    out.append("import {SigString} from std.dispatch;")
    if signed:
        out.append("import {sdiv, smod, sgt, signextend, mstore, sstore, sload} from std.opcodes;")
    else:
        out.append("import {mstore, sstore, sload} from std.opcodes;")
    out.append("")
    out.append("export { " + ", ".join(n for n, _ in types) + " };")
    out.append("")
    for name, bits in types:
        out.append(gen_type(name, bits, signed))
    with open(path, "w") as f:
        f.write("\n".join(out).rstrip() + "\n")
    print(f"wrote {path} ({len(types)} types)")


if __name__ == "__main__":
    import os

    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    gen_module(os.path.join(root, "std", "signed.sol"),
               "std.signed: signed integer types int8..int256.", SIGNED, True)
    gen_module(os.path.join(root, "std", "unsigned.sol"),
               "std.unsigned: unsigned integer types uint8..uint128 (uint256 is in std).", UNSIGNED, False)
