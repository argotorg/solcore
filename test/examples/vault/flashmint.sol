// Style 3: interface + callback (dependency inversion), as its own module.
import * from std;
import {mload} from std.opcodes;

export { callBorrower, flashOk };

// FLASH_OK = keccak256("IFlashBorrower.onFlashMint").
function flashOk() returns (word) { return keccakLit("IFlashBorrower.onFlashMint"); }

// Call borrower.onFlashMint(initiator, amount) and require the magic return value.
function callBorrower(borrower : address, initiator : address, amount : uint256) returns (()) {
  let sel : bytes32 = bytes32(0x85b2c20a00000000000000000000000000000000000000000000000000000000);
  let payload = concat(truncate(to_bytes(sel), 4),
                       concat(bytes32(Typedef.rep(initiator)), bytes32(Typedef.rep(amount))));
  match (raw_call(borrower, uint256(0), payload)) {
    case (ok, ret) {
      require(ok, Error(0x58e9a641));                                         // FlashCallReverted
      require(mload(MemoryPointer.ptr(ret)) == flashOk(), Error(0x003554dd)); // FlashBadReturn
    }
  }
}
