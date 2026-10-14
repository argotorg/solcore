// ERC20 core capability, with its own storage (three sub-slots of an ERC-7201 root).

import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;
import * from store;

export { HasLedger, HasSupply, HasAllowance, coreUpdate, spendAllowance, approveVal };

// Read-only capabilities (the setters are intentionally NOT part of the trait).
trait HasLedger<self> {
  function balanceOf(s : self, a : address) returns (uint256);
}

trait HasSupply<self> {
  function totalSupply(s : self) returns (uint256);
}

trait HasAllowance<self> {
  function allowanceOf(s : self, o : address, sp : address) returns (uint256);
}

function erc20Root() returns (word) { 
  return Typedef.rep(erc7201("mytoken.storage.ERC20")); 
}

function balancesBase() returns (word) { 
  return erc20Root(); 
}

function allowancesBase() returns (word) { 
  return erc20Root() + 1; 
}

function totalSupplySlot() returns (word) { 
  return erc20Root() + 2; 
}

function balanceSlot(a : address) returns (word) { 
  return hash2(balancesBase(), Typedef.rep(a)); 
}

function allowanceSlot(o : address, sp : address) returns (word) {
  return hash2(hash2(allowancesBase(), Typedef.rep(o)), Typedef.rep(sp));
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

impl HasAllowance<AppStore> {
  function allowanceOf(s : AppStore, o : address, sp : address) returns (uint256) {
    return uint256(sload(allowanceSlot(o, sp)));
  }
}

// Raw storage writes — module-private (not exported). The only callers are the
// sanctioned mutators below, so the ledger cannot be written any other way.
function writeBalance(a : address, v : uint256) returns (()) { 
  sstore(balanceSlot(a), Typedef.rep(v)); 
}

function writeSupply(v : uint256) returns (()) { 
  sstore(totalSupplySlot(), Typedef.rep(v)); 
}

function writeAllowance(o : address, sp : address, v : uint256) returns (()) {
  sstore(allowanceSlot(o, sp), Typedef.rep(v));
}

function emitTransfer(from : address, to : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Transfer(address,address,uint256)"), Typedef.rep(from), Typedef.rep(to));
}

function emitApproval(o : address, sp : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Approval(address,address,uint256)"), Typedef.rep(o), Typedef.rep(sp));
}

// The single sanctioned ledger mutator (the ERC20 `_update` seam): mint
// (from == 0), burn (to == 0) or move. Reads through the capability, writes
// through the private helpers.
function coreUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasLedger, self: HasSupply {
  if (from == address(0)) {
    let ts : uint256 = HasSupply.totalSupply(s);
    let nts : uint256 = Num.add(ts, value);
    require(nts >= ts, Error(0x4e487b71));       // overflow -> Panic(0x11)-like
    writeSupply(nts);
  } else {
    let bf : uint256 = HasLedger.balanceOf(s, from);
    require(bf >= value, Error(0xe450d38c));     // ERC20InsufficientBalance
    writeBalance(from, Num.sub(bf, value));
  }
  if (to == address(0)) {
    let ts2 : uint256 = HasSupply.totalSupply(s);
    writeSupply(Num.sub(ts2, value));
  } else {
    let bt : uint256 = HasLedger.balanceOf(s, to);
    writeBalance(to, Num.add(bt, value));
  }
  emitTransfer(from, to, value);
}

function spendAllowance<self>(s : self, o : address, sp : address, value : uint256) returns (())
  where self: HasAllowance {
  let cur : uint256 = HasAllowance.allowanceOf(s, o, sp);
  if (cur != Num.maxVal()) {
    require(cur >= value, Error(0xfb8f41b2));    // ERC20InsufficientAllowance
    writeAllowance(o, sp, Num.sub(cur, value));
  }
}

function approveVal<self>(s : self, o : address, sp : address, value : uint256) returns (())
  where self: HasAllowance {
  writeAllowance(o, sp, value);
  emitApproval(o, sp, value);
}
