import std.{*};
import std.opcodes.{mstore, keccak256, shl};

export {
  eip712Digest,
  eip712DomainSeparator
};

// --- EIP-712 (typed structured data hashing & signing) ---
// https://eips.ethereum.org/EIPS/eip-712
//
// The digest a wallet signs is
//   keccak256(0x19 0x01 ‖ domainSeparator ‖ hashStruct(message))
// where every `hashStruct(s)` is `keccak256(typeHash ‖ encodeData(s))` and the
// domain separator is the `hashStruct` of the standard EIP712Domain struct.
//
// Encoding a struct's members is application specific (it depends on which
// members the struct has and whether they are atomic or dynamic), so the
// message struct hash is built by the caller. The two reusable pieces live
// here: the domain separator for the common `EIP712Domain(string name,string
// version,uint256 chainId,address verifyingContract)` shape, and the `0x1901`
// digest combinator that binds a domain separator to a message struct hash.

// hashStruct of the standard EIP712Domain. `nameHash` / `versionHash` are the
// keccak256 of the (dynamic) name / version strings — typically compile-time
// constants produced with `keccakLit`. `chainId` / `verifyingContract` are
// encoded as their left-padded 32-byte words.
function eip712DomainSeparator(
    nameHash: bytes32,
    versionHash: bytes32,
    chainId: uint256,
    verifyingContract: address
) -> bytes32 {
    let typeHash = keccakLit("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)");
    // Lay the five 32-byte words out contiguously and hash them. We borrow the
    // area above the free-memory pointer as scratch (as `ecrecover` does): the
    // preimage is consumed immediately by keccak256 and never needs to persist,
    // so there is no need to bump the free pointer.
    let ptr = get_free_memory();
    mstore(ptr,       typeHash);
    mstore(ptr + 32,  Typedef.rep(nameHash));
    mstore(ptr + 64,  Typedef.rep(versionHash));
    mstore(ptr + 96,  Typedef.rep(chainId));
    mstore(ptr + 128, Typedef.rep(verifyingContract));
    return bytes32(keccak256(ptr, 160));
}

// Binds a domain separator to a message's struct hash, yielding the final
// EIP-712 digest: keccak256(0x19 0x01 ‖ domainSeparator ‖ structHash). The
// two-byte 0x1901 prefix occupies the leading bytes of the first word.
function eip712Digest(domainSeparator: bytes32, structHash: bytes32) -> bytes32 {
    let ptr = get_free_memory();
    mstore(ptr,      shl(240, 0x1901));            // 0x1901 in the leading two bytes
    mstore(ptr + 2,  Typedef.rep(domainSeparator));
    mstore(ptr + 34, Typedef.rep(structHash));
    return bytes32(keccak256(ptr, 66));
}
