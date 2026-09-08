// Pausable capability, with its own storage.
import * from std;
import {sload, sstore} from std.opcodes;
import * from store;

export { HasPaused, whenNotPaused };

trait HasPaused<self> {
  function isPaused(s : self) returns (uint256);
  function setPaused(s : self, p : uint256) returns (());
}

function pausedSlot() returns (word) { return Typedef.rep(erc7201("valt.storage.Pausable")); }

impl HasPaused<AppStore> {
  function isPaused(s : AppStore) returns (uint256) { return uint256(sload(pausedSlot())); }
  function setPaused(s : AppStore, p : uint256) returns (()) { sstore(pausedSlot(), Typedef.rep(p)); }
}

function whenNotPaused<self>(s : self) returns (()) where self: HasPaused {
  require(HasPaused.isPaused(s) == uint256(0), Error(0xd93c0665));
}
