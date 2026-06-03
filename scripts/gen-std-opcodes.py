#!/usr/bin/env python3
"""Generate std/opcodes.solc from a declarative list of EVM opcodes."""

import os
from string import ascii_lowercase

opcodes = [
    # 0x00-0x0b: stop & arithmetic
    {"name": "stop",           "inputs": 0, "output": 0},
    {"name": "add",            "inputs": 2, "output": 1},
    {"name": "mul",            "inputs": 2, "output": 1},
    {"name": "sub",            "inputs": 2, "output": 1},
    {"name": "div",            "inputs": 2, "output": 1},
    {"name": "sdiv",           "inputs": 2, "output": 1},
    {"name": "mod",            "inputs": 2, "output": 1},
    {"name": "smod",           "inputs": 2, "output": 1},
    {"name": "addmod",         "inputs": 3, "output": 1},
    {"name": "mulmod",         "inputs": 3, "output": 1},
    {"name": "exp",            "inputs": 2, "output": 1},
    {"name": "signextend",     "inputs": 2, "output": 1},
    # 0x10-0x1d: comparison & bitwise
    {"name": "lt",             "inputs": 2, "output": 1},
    {"name": "gt",             "inputs": 2, "output": 1},
    {"name": "slt",            "inputs": 2, "output": 1},
    {"name": "sgt",            "inputs": 2, "output": 1},
    {"name": "eq",             "inputs": 2, "output": 1},
    {"name": "iszero",         "inputs": 1, "output": 1},
    {"name": "and",            "inputs": 2, "output": 1},
    {"name": "or",             "inputs": 2, "output": 1},
    {"name": "xor",            "inputs": 2, "output": 1},
    {"name": "not",            "inputs": 1, "output": 1},
    {"name": "byte",           "inputs": 2, "output": 1},
    {"name": "shl",            "inputs": 2, "output": 1},
    {"name": "shr",            "inputs": 2, "output": 1},
    {"name": "sar",            "inputs": 2, "output": 1},
    # 0x20: keccak
    {"name": "keccak256",      "inputs": 2, "output": 1},
    # 0x30-0x3f: environment
    {"name": "address",        "inputs": 0, "output": 1},
    {"name": "balance",        "inputs": 1, "output": 1},
    {"name": "origin",         "inputs": 0, "output": 1},
    {"name": "caller",         "inputs": 0, "output": 1},
    {"name": "callvalue",      "inputs": 0, "output": 1},
    {"name": "calldataload",   "inputs": 1, "output": 1},
    {"name": "calldatasize",   "inputs": 0, "output": 1},
    {"name": "calldatacopy",   "inputs": 3, "output": 0},
    {"name": "codesize",       "inputs": 0, "output": 1},
    {"name": "codecopy",       "inputs": 3, "output": 0},
    {"name": "gasprice",       "inputs": 0, "output": 1},
    {"name": "extcodesize",    "inputs": 1, "output": 1},
    {"name": "extcodecopy",    "inputs": 4, "output": 0},
    {"name": "returndatasize", "inputs": 0, "output": 1},
    {"name": "returndatacopy", "inputs": 3, "output": 0},
    {"name": "extcodehash",    "inputs": 1, "output": 1},
    # 0x40-0x4a: block information
    {"name": "blockhash",      "inputs": 1, "output": 1},
    {"name": "coinbase",       "inputs": 0, "output": 1},
    {"name": "timestamp",      "inputs": 0, "output": 1},
    {"name": "number",         "inputs": 0, "output": 1},
    {"name": "prevrandao",     "inputs": 0, "output": 1},
    {"name": "gaslimit",       "inputs": 0, "output": 1},
    {"name": "chainid",        "inputs": 0, "output": 1},
    {"name": "selfbalance",    "inputs": 0, "output": 1},
    {"name": "basefee",        "inputs": 0, "output": 1},
    {"name": "blobhash",       "inputs": 1, "output": 1},
    {"name": "blobbasefee",    "inputs": 0, "output": 1},
    # 0x50-0x5e: stack, memory, storage (jump/jumpi/pc/jumpdest are not Yul builtins)
    {"name": "pop",            "inputs": 1, "output": 0},
    {"name": "mload",          "inputs": 1, "output": 1},
    {"name": "mstore",         "inputs": 2, "output": 0},
    {"name": "mstore8",        "inputs": 2, "output": 0},
    {"name": "sload",          "inputs": 1, "output": 1},
    {"name": "sstore",         "inputs": 2, "output": 0},
    {"name": "msize",          "inputs": 0, "output": 1},
    {"name": "gas",            "inputs": 0, "output": 1},
    {"name": "tload",          "inputs": 1, "output": 1},
    {"name": "tstore",         "inputs": 2, "output": 0},
    {"name": "mcopy",          "inputs": 3, "output": 0},
    # 0xa0-0xa4: logging
    {"name": "log0",           "inputs": 2, "output": 0},
    {"name": "log1",           "inputs": 3, "output": 0},
    {"name": "log2",           "inputs": 4, "output": 0},
    {"name": "log3",           "inputs": 5, "output": 0},
    {"name": "log4",           "inputs": 6, "output": 0},
    # 0xf0-0xff: system
    {"name": "create",         "inputs": 3, "output": 1},
    {"name": "call",           "inputs": 7, "output": 1},
    {"name": "callcode",       "inputs": 7, "output": 1},
    {"name": "return",         "inputs": 2, "output": 0},
    {"name": "delegatecall",   "inputs": 6, "output": 1},
    {"name": "create2",        "inputs": 4, "output": 1},
    {"name": "staticcall",     "inputs": 6, "output": 1},
    {"name": "revert",         "inputs": 2, "output": 0},
    {"name": "invalid",        "inputs": 0, "output": 0},
    {"name": "selfdestruct",   "inputs": 1, "output": 0},
]


# Opcodes whose names clash with solcore keywords get a trailing underscore
# in the wrapper function name, while the inner assembly call still uses the
# real EVM mnemonic.
RESERVED_NAMES = {"return": "return_"}


def wrapper_name(op):
    return RESERVED_NAMES.get(op["name"], op["name"])


def arg_names(n):
    return [ascii_lowercase[i] for i in range(n)]


def gen_export(ops):
    lines = ["export {"]
    for i, op in enumerate(ops):
        sep = "," if i < len(ops) - 1 else ""
        lines.append(f"    {wrapper_name(op)}{sep}")
    lines.append("};")
    return "\n".join(lines)


def gen_function(op):
    name = op["name"]
    fname = wrapper_name(op)
    args = arg_names(op["inputs"])
    params = ", ".join(f"{a}: word" for a in args)
    call_args = ", ".join(args)
    ret_type = "word" if op["output"] == 1 else "()"

    lines = [f"function {fname}({params}) -> {ret_type} {{"]
    if op["output"] == 1:
        lines.append("    let res;")
        lines.append("    assembly {")
        lines.append(f"        res := {name}({call_args})")
        lines.append("    }")
        lines.append("    return res;")
    else:
        lines.append("    assembly {")
        lines.append(f"        {name}({call_args})")
        lines.append("    }")
    lines.append("}")
    return "\n".join(lines)


def render(ops):
    parts = [gen_export(ops)]
    for op in ops:
        parts.append("")
        parts.append(gen_function(op))
    return "\n".join(parts) + "\n"


def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    out_path = os.path.join(script_dir, os.pardir, "std", "opcodes.solc")
    with open(out_path, "w") as f:
        f.write(render(opcodes))


if __name__ == "__main__":
    main()
