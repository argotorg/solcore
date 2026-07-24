import std.{*};
import std.dispatch.{*};
import std.opcodes.{mload, mstore};

contract MemberRegistry {
  members : array(address);

  constructor() {}

  public function addMember(addr : address) -> () {
    ArrayPush.push(members, addr);
  }

  // MemberNotFound() selector
  public function removeMember(addr : address) -> () {
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

  public function numberOfMembers() -> uint256 {
    return Length.length(members);
  }

  public function getMembers() -> memory(DynArray(address)) {
    let count : word = Typedef.rep(Length.length(members));
    let totalBytes : word = (count + 1) * 32;
    let ptr : word = allocate_memory(totalBytes);
    mstore(ptr, count);

    let i : word;
    for (i = 0; i < count; i = i + 1) {
      let addr : address = members[uint256(i)];
      mstore(ptr + 32 + i * 32, Typedef.rep(addr));
    }
    return Typedef.abs(ptr) : memory(DynArray(address));
  }
}
