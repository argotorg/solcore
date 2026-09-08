// ERC20Base capability, with its own storage (balances at root+0, totalSupply at root+1).
//
// Encapsulation: the traits expose READ-ONLY accessors; the raw slot writes are
// module-private (not exported), so `coreUpdate` is the only sanctioned way to
// mutate the ledger — the guarantee OpenZeppelin gets from a `private _balances`.
import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;
import * from store;
import * from mathlib;

export { HasLedger, HasSupply, coreUpdate, emitTransfer };

trait HasLedger<self> {
  function balanceOf(s : self, a : address) returns (uint256);
}

trait HasSupply<self> {
  function totalSupply(s : self) returns (uint256);
}

function erc20Root() returns (word) { 
  return Typedef.rep(erc7201("vault.storage.ERC20")); 
}

function balancesBase() returns (word) { 
  return erc20Root(); 
}

function totalSupplySlot() returns (word) { 
  return erc20Root() + 1; 
}

function balanceSlot(a : address) returns (word) { 
  return hash2(balancesBase(), Typedef.rep(a)); 
}

impl HasLedger<AppStore> {
  function balanceOf(s : AppStore, a : address) returns (uint256) { 
    return uint256(sload(balanceSlot(a))); 
  }
}

impl HasSupply<AppStore> {
  function totalSupply(s : AppStore) returns (uint256) { 
    return uint256(sload(totalSupplySlot())); 
  }
}

// Raw storage writes — module-private (not exported); only coreUpdate calls them.
function writeBalance(a : address, v : uint256) returns (()) { 
  sstore(balanceSlot(a), Typedef.rep(v)); 
}

function writeSupply(v : uint256) returns (()) { 
  sstore(totalSupplySlot(), Typedef.rep(v)); 
}

function emitTransfer(from : address, to : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Transfer(address,address,uint256)"), Typedef.rep(from), Typedef.rep(to));
}

// The single sanctioned ledger mutator (the ERC20Base `_update` hook): mint
// (from == 0), burn (to == 0) or move, with over/underflow checked by mathlib.
function coreUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasLedger, self: HasSupply {
  if (from == address(0)) {
    writeSupply(addChecked(HasSupply.totalSupply(s), value));
  } else {
    writeBalance(from, subChecked(HasLedger.balanceOf(s, from), value));
  }
  if (to == address(0)) {
    writeSupply(subChecked(HasSupply.totalSupply(s), value));
  } else {
    writeBalance(to, addChecked(HasLedger.balanceOf(s, to), value));
  }
  emitTransfer(from, to, value);
}
