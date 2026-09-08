// ERC20Base capability, with its own storage (balances at root+0, totalSupply at root+1).
import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;
import * from store;
import * from mathlib;

export { HasLedger, HasSupply, coreUpdate, emitTransfer };

trait HasLedger<self> {
  function balanceOf(s : self, a : address) returns (uint256);
  function setBalance(s : self, a : address, v : uint256) returns (());
}

trait HasSupply<self> {
  function totalSupply(s : self) returns (uint256);
  function setTotalSupply(s : self, v : uint256) returns (());
}

function erc20Root() returns (word) { return Typedef.rep(erc7201("valt.storage.ERC20")); }
function balancesBase() returns (word) { return erc20Root(); }
function totalSupplySlot() returns (word) { return erc20Root() + 1; }
function balanceSlot(a : address) returns (word) { return hash2(balancesBase(), Typedef.rep(a)); }

impl HasLedger<AppStore> {
  function balanceOf(s : AppStore, a : address) returns (uint256) { return uint256(sload(balanceSlot(a))); }
  function setBalance(s : AppStore, a : address, v : uint256) returns (()) { sstore(balanceSlot(a), Typedef.rep(v)); }
}

impl HasSupply<AppStore> {
  function totalSupply(s : AppStore) returns (uint256) { return uint256(sload(totalSupplySlot())); }
  function setTotalSupply(s : AppStore, v : uint256) returns (()) { sstore(totalSupplySlot(), Typedef.rep(v)); }
}

function emitTransfer(from : address, to : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Transfer(address,address,uint256)"), Typedef.rep(from), Typedef.rep(to));
}

function coreUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasLedger, self: HasSupply {
  if (from == address(0)) {
    HasSupply.setTotalSupply(s, addChecked(HasSupply.totalSupply(s), value));
  } else {
    HasLedger.setBalance(s, from, subChecked(HasLedger.balanceOf(s, from), value));
  }
  if (to == address(0)) {
    HasSupply.setTotalSupply(s, subChecked(HasSupply.totalSupply(s), value));
  } else {
    HasLedger.setBalance(s, to, addChecked(HasLedger.balanceOf(s, to), value));
  }
  emitTransfer(from, to, value);
}
