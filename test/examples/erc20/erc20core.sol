// ERC20 core capability, with its own storage (three sub-slots of an ERC-7201 root).
import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;
import * from store;

export { HasLedger, HasSupply, HasAllowance, coreUpdate, spendAllowance, approveVal };

trait HasLedger<self> {
  function balanceOf(s : self, a : address) returns (uint256);
  function setBalance(s : self, a : address, v : uint256) returns (());
}

trait HasSupply<self> {
  function totalSupply(s : self) returns (uint256);
  function setTotalSupply(s : self, v : uint256) returns (());
}

trait HasAllowance<self> {
  function allowanceOf(s : self, o : address, sp : address) returns (uint256);
  function setAllowance(s : self, o : address, sp : address, v : uint256) returns (());
}

function erc20Root() returns (word) { return Typedef.rep(erc7201("mytoken.storage.ERC20")); }
function balancesBase() returns (word) { return erc20Root(); }
function allowancesBase() returns (word) { return erc20Root() + 1; }
function totalSupplySlot() returns (word) { return erc20Root() + 2; }
function balanceSlot(a : address) returns (word) { return hash2(balancesBase(), Typedef.rep(a)); }
function allowanceSlot(o : address, sp : address) returns (word) {
  return hash2(hash2(allowancesBase(), Typedef.rep(o)), Typedef.rep(sp));
}

impl HasLedger<AppStore> {
  function balanceOf(s : AppStore, a : address) returns (uint256) { return uint256(sload(balanceSlot(a))); }
  function setBalance(s : AppStore, a : address, v : uint256) returns (()) { sstore(balanceSlot(a), Typedef.rep(v)); }
}

impl HasSupply<AppStore> {
  function totalSupply(s : AppStore) returns (uint256) { return uint256(sload(totalSupplySlot())); }
  function setTotalSupply(s : AppStore, v : uint256) returns (()) { sstore(totalSupplySlot(), Typedef.rep(v)); }
}

impl HasAllowance<AppStore> {
  function allowanceOf(s : AppStore, o : address, sp : address) returns (uint256) {
    return uint256(sload(allowanceSlot(o, sp)));
  }
  function setAllowance(s : AppStore, o : address, sp : address, v : uint256) returns (()) {
    sstore(allowanceSlot(o, sp), Typedef.rep(v));
  }
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

function coreUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasLedger, self: HasSupply {
  if (from == address(0)) {
    let ts : uint256 = HasSupply.totalSupply(s);
    let nts : uint256 = Num.add(ts, value);
    require(nts >= ts, Error(0x4e487b71));       // overflow -> Panic(0x11)-like
    HasSupply.setTotalSupply(s, nts);
  } else {
    let bf : uint256 = HasLedger.balanceOf(s, from);
    require(bf >= value, Error(0xe450d38c));     // ERC20InsufficientBalance
    HasLedger.setBalance(s, from, Num.sub(bf, value));
  }
  if (to == address(0)) {
    let ts2 : uint256 = HasSupply.totalSupply(s);
    HasSupply.setTotalSupply(s, Num.sub(ts2, value));
  } else {
    let bt : uint256 = HasLedger.balanceOf(s, to);
    HasLedger.setBalance(s, to, Num.add(bt, value));
  }
  emitTransfer(from, to, value);
}

function spendAllowance<self>(s : self, o : address, sp : address, value : uint256) returns (())
  where self: HasAllowance {
  let cur : uint256 = HasAllowance.allowanceOf(s, o, sp);
  if (cur != Num.maxVal()) {
    require(cur >= value, Error(0xfb8f41b2));    // ERC20InsufficientAllowance
    HasAllowance.setAllowance(s, o, sp, Num.sub(cur, value));
  }
}

function approveVal<self>(s : self, o : address, sp : address, value : uint256) returns (())
  where self: HasAllowance {
  HasAllowance.setAllowance(s, o, sp, value);
  emitApproval(o, sp, value);
}
