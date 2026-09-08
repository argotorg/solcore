// Permit capability (EIP-2612), with its own storage.
import * from std;
import {sload, sstore} from std.opcodes;
import * from std.eip712;
import * from store;
import * from erc20core;

export { HasNonces, useNonce, runPermit, permitDomainSeparator };

trait HasNonces<self> {
  function nonceOf(s : self, owner : address) returns (uint256);
  function setNonce(s : self, owner : address, v : uint256) returns (());
}

function noncesRoot() returns (word) { return Typedef.rep(erc7201("mytoken.storage.Permit")); }
function nonceSlot(owner : address) returns (word) { return hash2(noncesRoot(), Typedef.rep(owner)); }

impl HasNonces<AppStore> {
  function nonceOf(s : AppStore, owner : address) returns (uint256) { return uint256(sload(nonceSlot(owner))); }
  function setNonce(s : AppStore, owner : address, v : uint256) returns (()) { sstore(nonceSlot(owner), Typedef.rep(v)); }
}

function useNonce<self>(s : self, owner : address) returns (uint256) where self: HasNonces {
  let cur : uint256 = HasNonces.nonceOf(s, owner);
  HasNonces.setNonce(s, owner, Num.add(cur, uint256(1)));
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

function runPermit<self>(s : self, owner : address, spender : address, value : uint256,
                   deadline : uint256, currentTime : uint256, chainId : uint256,
                   verifyingContract : address, v : uint256, r : bytes32, sSig : bytes32) returns (())
  where self: HasNonces, self: HasAllowance {
  require(currentTime <= deadline, Error(0x62791302));   // ERC2612ExpiredSignature
  let nonce : uint256 = useNonce(s, owner);
  let typeHash : bytes32 =
    bytes32(keccakLit("Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)"));
  let structHash : bytes32 = keccak256_(
    concat(typeHash,
    concat(bytes32(Typedef.rep(owner)),
    concat(bytes32(Typedef.rep(spender)),
    concat(bytes32(Typedef.rep(value)),
    concat(bytes32(Typedef.rep(nonce)),
           bytes32(Typedef.rep(deadline)))))))
  );
  let digest : bytes32 = eip712Digest(permitDomainSeparator(verifyingContract, chainId), structHash);
  let signer : address = ecrecover(digest, v, r, sSig);
  require(signer == owner, Error(0x4b800e46));           // ERC2612InvalidSigner
  approveVal(s, owner, spender, value);
}
