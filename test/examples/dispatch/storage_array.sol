import {*} from std;
import {*} from std.dispatch;
import {mload, mstore} from std.opcodes;

contract MemberRegistry {
  members : address[];

  constructor() {}

  function addMember(addr : address) public returns (()) {
    ArrayPush.push(members, addr);
  }

  // MemberNotFound() selector
  function removeMember(addr : address) public returns (()) {
    // foundIdx == length() acts as the "not found" sentinel.
    let foundIdx : uint256 = Length.length(members);
    let i : uint256;
    for (i = uint256(0); i < Length.length(members); i = i + uint256(1)) {
      if (members[i] == addr) {
        // NOTE: solcore has no `break`, so we keep scanning.
        foundIdx = i;
      }
    }
    require(foundIdx != Length.length(members), Error(0xdeadbeef));

    // Shift subsequent elements down one slot to close the gap.
    for (; foundIdx < Length.length(members) - uint256(1); foundIdx = foundIdx + uint256(1)) {
      members[foundIdx] = members[foundIdx + uint256(1)];
    }
    // Drop the (now-duplicated) last item and adjust the length.
    Array.pop(members);
  }

  function numberOfMembers() public returns (uint256) {
    return Length.length(members);
  }

  function getMembers() public returns (DynArray<address> memory) {
    let count : word = Typedef.rep(Length.length(members));
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
