// Current voting power (ERC-5805-style) at ERC-7201 namespaced slots, plus a
// TransferHook that moves voting power between delegates on every transfer.
import * from std;
import {sload, sstore, log4} from std.opcodes;
import * from option;
import * from hooks;
import {balance} from erc20core;

export { getDelegate, votesOf, delegateTo, Votes(*) };

function delegateeSlot(a : address) returns (word) {
  return hash2(Typedef.rep(erc7201("mytoken.hooks.votes.delegate")), Typedef.rep(a));
}

function votesSlot(a : address) returns (word) {
  return hash2(Typedef.rep(erc7201("mytoken.hooks.votes.power")), Typedef.rep(a));
}

function getDelegate(a : address) returns (address) {
  return address(sload(delegateeSlot(a)));
}

// module-private writers (not exported)
function setDelegate(a : address, d : address) returns (()) {
  sstore(delegateeSlot(a), Typedef.rep(d));
}

function votesOf(a : address) returns (uint256) {
  return uint256(sload(votesSlot(a)));
}

function setVotes(a : address, v : uint256) returns (()) {
  sstore(votesSlot(a), Typedef.rep(v));
}

function moveVotingPower(fromRep : address, toRep : address, amt : uint256) returns (()) {
  if (fromRep != toRep) {
    if (fromRep != address(0)) {
      setVotes(fromRep, Num.sub(votesOf(fromRep), amt));
    }
    if (toRep != address(0)) {
      setVotes(toRep, Num.add(votesOf(toRep), amt));
    }
  }
}

function emitDelegateChanged(delegator : address, fromD : address, toD : address) returns (()) {
  log4(0, 0, keccakLit("DelegateChanged(address,address,address)"), Typedef.rep(delegator), Typedef.rep(fromD), Typedef.rep(toD));
}

function delegateTo(account : address, delegatee : address) returns (()) {
  let old : address = getDelegate(account);
  setDelegate(account, delegatee);
  emitDelegateChanged(account, old, delegatee);
  moveVotingPower(old, delegatee, balance(account));
}

// Transfer hook: move amount of voting power from the sender's delegate to
// the recipient's delegate (None maps to the zero address, i.e. mint/burn).
enum Votes { Votes }

impl TransferHook<Votes> {
  function on(hook : Votes, from : Option<address>, to : Option<address>, amount : uint256) returns (()) {
    moveVotingPower(getDelegate(unwrapOr(from, address(0))), getDelegate(unwrapOr(to, address(0))), amount);
  }
}
