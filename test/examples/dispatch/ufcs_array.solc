import {*} from std;
import {*} from std.dispatch;
import {mload, mstore} from std.opcodes;

// UFCS counterpart of storage_array.solc.
//
// This contract is byte-for-byte equivalent in behaviour to
// dispatch/storage_array.solc, but exercises the receiver-style method-call
// sugar resolved by NameResolution: when the receiver of recv.method(args)
// is a runtime value and a unique class exposes method, the call is rewritten
// to Class.method(recv, args). Here each receiver is a contract field, so:
//
//   members.push(addr)  ==>  ArrayPush.push(members, addr)
//   members.length()    ==>  Length.length(members)
//   members.pop()       ==>  Array.pop(members)
//
// It runs against the SAME assertions as storage_array.json (see
// ufcs_array.json), proving UFCS and the explicit qualified calls compile to
// the same runtime behaviour.  Indexed access members[i] is unaffecte: it
// is handled by field-access desugaring, not UFCS.
contract MemberRegistry {
  members : address[];

  constructor() {}

  function addMember(addr : address) public returns (()) {
    members.push(addr);
  }

  // MemberNotFound() selector
  function removeMember(addr : address) public returns (()) {
    // foundIdx == length() acts as the "not found" sentinel.
    let foundIdx : uint256 = members.length();
    let i : uint256;
    for (i = uint256(0); i < members.length(); i = i + uint256(1)) {
      if (members[i] == addr) {
        // NOTE: solcore has no break, so we keep scanning.
        foundIdx = i;
      }
    }
    require(foundIdx != members.length(), Error(0xdeadbeef));

    // Shift subsequent elements down one slot to close the gap.
    for (; foundIdx < members.length() - uint256(1); foundIdx = foundIdx + uint256(1)) {
      members[foundIdx] = members[foundIdx + uint256(1)];
    }
    // Drop the (now-duplicated) last item and adjust the length.
    members.pop();
  }

  function numberOfMembers() public returns (uint256) {
    return members.length();
  }

  function getMembers() public returns (DynArray<address> memory) {
    let count : word = Typedef.rep(members.length());
    let totalBytes : word = (count + 1) * 32;
    let ptr : word = allocate_memory(totalBytes);
    mstore(ptr, count);

    let i : word;
    for (i = 0; i < count; i = i + 1) {
      let addr : address = members[uint256(i)];
      mstore(ptr + 32 + i * 32, Typedef.rep(addr));
    }
    return Typedef.abs(ptr) as DynArray<address> memory;
  }
}
