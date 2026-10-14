// Owner address at an ERC-7201 namespaced slot, as free functions.
import * from std;
import {sload, sstore} from std.opcodes;
import {sender} from context;

export { owner, initOwner, requireOwner, setOwner };

function ownerSlot() returns (word) {
  return Typedef.rep(erc7201("mytoken.hooks.ownable"));
}

function owner() returns (address) {
  return address(sload(ownerSlot()));
}

function initOwner(who : address) returns (()) {
  sstore(ownerSlot(), Typedef.rep(who));
}

function setOwner(to : address) returns (()) {
  sstore(ownerSlot(), Typedef.rep(to));
}

function requireOwner() returns (()) {
  require(sender() == owner(), Error(0x118cdaa7));   // OwnableUnauthorizedAccount
}
