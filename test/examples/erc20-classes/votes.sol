// Votes capability (current voting power), with its own storage.
import * from std;
import {sload, sstore, log4} from std.opcodes;
import * from store;
import * from erc20core;

export { HasVotes, delegateTo, moveVotingPower, votesOnTransfer };

trait HasVotes<self> {
  function getDelegate(s : self, account : address) returns (address);
  function setDelegate(s : self, account : address, d : address) returns (());
  function votesOf(s : self, account : address) returns (uint256);
  function setVotes(s : self, account : address, v : uint256) returns (());
}

function votesRoot() returns (word) { 
  return Typedef.rep(erc7201("mytoken.storage.Votes")); 
}

function delegateeSlot(a : address) returns (word) { 
  return hash2(votesRoot(), Typedef.rep(a)); 
}

function votesSlot(a : address) returns (word) { 
  return hash2(votesRoot() + 1, Typedef.rep(a)); 
}

impl HasVotes<AppStore> {
  function getDelegate(s : AppStore, account : address) returns (address) {
    return address(sload(delegateeSlot(account)));
  }
  function setDelegate(s : AppStore, account : address, d : address) returns (()) {
    sstore(delegateeSlot(account), Typedef.rep(d));
  }
  function votesOf(s : AppStore, account : address) returns (uint256) {
    return uint256(sload(votesSlot(account)));
  }
  function setVotes(s : AppStore, account : address, v : uint256) returns (()) {
    sstore(votesSlot(account), Typedef.rep(v));
  }
}

function moveVotingPower<self>(s : self, fromRep : address, toRep : address, amt : uint256) returns (())
  where self: HasVotes {
  if (fromRep != toRep) {
    if (fromRep != address(0)) {
      HasVotes.setVotes(s, fromRep, Num.sub(HasVotes.votesOf(s, fromRep), amt));
    }
    if (toRep != address(0)) {
      HasVotes.setVotes(s, toRep, Num.add(HasVotes.votesOf(s, toRep), amt));
    }
  }
}

function emitDelegateChanged(delegator : address, fromD : address, toD : address) returns (()) {
  log4(0, 0, keccakLit("DelegateChanged(address,address,address)"), Typedef.rep(delegator), Typedef.rep(fromD), Typedef.rep(toD));
}

function delegateTo<self>(s : self, account : address, delegatee : address) returns (())
  where self: HasVotes, self: HasLedger {
  let old : address = HasVotes.getDelegate(s, account);
  HasVotes.setDelegate(s, account, delegatee);
  emitDelegateChanged(account, old, delegatee);
  moveVotingPower(s, old, delegatee, HasLedger.balanceOf(s, account));
}

function votesOnTransfer<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasVotes {
  moveVotingPower(s, HasVotes.getDelegate(s, from), HasVotes.getDelegate(s, to), value);
}
