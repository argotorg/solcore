// Ownable capability, with its own storage.
import * from std;
import {sload, sstore} from std.opcodes;
import * from store;

export { HasOwner, requireOwner };

trait HasOwner<self> {
  function getOwner(s : self) returns (address);
  function setOwner(s : self, o : address) returns (());
}

function ownerSlot() returns (word) { return Typedef.rep(erc7201("vault.storage.Ownable")); }

impl HasOwner<AppStore> {
  function getOwner(s : AppStore) returns (address) { return address(sload(ownerSlot())); }
  function setOwner(s : AppStore, o : address) returns (()) { sstore(ownerSlot(), Typedef.rep(o)); }
}

function requireOwner<self>(s : self, sender : address) returns (()) where self: HasOwner {
  require(sender == HasOwner.getOwner(s), Error(0x118cdaa7));
}
