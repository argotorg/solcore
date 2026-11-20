// Pause flag at an ERC-7201 namespaced slot, plus a TransferHook that blocks
// every balance change while paused.
import * from std;
import {sload, sstore} from std.opcodes;
import * from option;
import * from hooks;

export { isPaused, setPaused, requireNotPaused, Pausable(*) };

function pausedSlot() returns (word) {
  return Typedef.rep(erc7201("mytoken.hooks.pausable"));
}

function isPaused() returns (uint256) {
  return uint256(sload(pausedSlot()));
}

function setPaused(p : uint256) returns (()) {
  sstore(pausedSlot(), Typedef.rep(p));
}

function requireNotPaused() returns (()) {
  require(isPaused() == uint256(0), Error(0xd93c0665));   // EnforcedPause
}

// Transfer hook: no balance may change while paused.
enum Pausable { Pausable }

impl TransferHook<Pausable> {
  function on(hook : Pausable, from : Option<address>, to : Option<address>, amount : uint256) returns (()) {
    requireNotPaused();
  }
}
