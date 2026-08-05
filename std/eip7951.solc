import std.{*};
import std.opcodes.{mstore, mload, gas, staticcall};

export { p256verify };

// Perform secp256r1 recovery and check using the P256VERIFY precompile at address
// 0x100, introduced by EIP-7951 / RIP-7212.
// Input layout (160 bytes): hash | r | s | qx | qy. The precompile
// returns a 32-byte word equal to 1 on a valid signature and empty output on an
// invalid one; we pre-clear the [0, 32) scratch slot so the failing case reads
// back as 0.
function p256verify(hash: bytes32, r: bytes32, s: bytes32, qx: bytes32, qy: bytes32) -> bool {
    let hash_ = Typedef.rep(hash);
    let r_ = Typedef.rep(r);
    let s_ = Typedef.rep(s);
    let qx_ = Typedef.rep(qx);
    let qy_ = Typedef.rep(qy);
    let ptr = get_free_memory();
    mstore(ptr, hash_);
    mstore(ptr + 32, r_);
    mstore(ptr + 64, s_);
    mstore(ptr + 96, qx_);
    mstore(ptr + 128, qy_);
    // We assume the [0, 32] scratch space is reserved.
    // Clear the scratch slot so an empty (verification-failed) response reads as 0.
    mstore(0, 0);
    let ret = staticcall(gas(), 0x100, ptr, 160, 0, 32);
    require(ret != 0, Error(0x1fb6bf04)); // P256VerifyCallFailed()
    // NOTE: we are doing the inverse check here for safety, so not using tobool()
    match mload(0) {
        | 1 => return true;
        | _ => return false;
    }
}
