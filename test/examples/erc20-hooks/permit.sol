// Permit capability (EIP-2612) at an ERC-7201 namespaced slot, as free
// functions. Sets an allowance from an off-chain signature.
import * from std;
import {sload, sstore} from std.opcodes;
import * from std.eip712;
import {approveAllowance} from allowance;

export { nonceOf, runPermit, permitDomainSeparator };

function noncesRoot() returns (word) {
  return Typedef.rep(erc7201("mytoken.hooks.permit"));
}

function nonceSlot(owner : address) returns (word) {
  return hash2(noncesRoot(), Typedef.rep(owner));
}

function nonceOf(owner : address) returns (uint256) {
  return uint256(sload(nonceSlot(owner)));
}

// module-private writer (not exported)
function setNonce(owner : address, v : uint256) returns (()) {
  sstore(nonceSlot(owner), Typedef.rep(v));
}

function useNonce(owner : address) returns (uint256) {
  let cur : uint256 = nonceOf(owner);
  setNonce(owner, Num.add(cur, uint256(1)));
  return cur;
}

function permitDomainSeparator(verifyingContract : address, chainId : uint256) returns (bytes32) {
  return eip712DomainSeparator(
    bytes32(keccakLit("MyToken")),
    bytes32(keccakLit("1")),
    chainId,
    verifyingContract
  );
}

function runPermit(owner : address, spender : address, value : uint256, deadline : uint256,
                   currentTime : uint256, chainId : uint256, verifyingContract : address,
                   v : uint256, r : bytes32, sSig : bytes32) returns (()) {
  require(currentTime <= deadline, Error(0x62791302));   // ERC2612ExpiredSignature
  let nonce : uint256 = useNonce(owner);
  let typeHash : bytes32 = bytes32(keccakLit("Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)"));
  // EIP-712 struct fields, each left-padded to 32 bytes.
  let ownerWord : bytes32 = bytes32(Typedef.rep(owner));
  let spenderWord : bytes32 = bytes32(Typedef.rep(spender));
  let valueWord : bytes32 = bytes32(Typedef.rep(value));
  let nonceWord : bytes32 = bytes32(Typedef.rep(nonce));
  let deadlineWord : bytes32 = bytes32(Typedef.rep(deadline));
  let encoded : memory<bytes> = concat(typeHash,
                                concat(ownerWord,
                                concat(spenderWord,
                                concat(valueWord,
                                concat(nonceWord, deadlineWord)))));
  let structHash : bytes32 = keccak256_(encoded);
  let digest : bytes32 = eip712Digest(permitDomainSeparator(verifyingContract, chainId), structHash);
  let signer : address = ecrecover(digest, v, r, sSig);
  require(signer == owner, Error(0x4b800e46));           // ERC2612InvalidSigner
  approveAllowance(owner, spender, value);
}
