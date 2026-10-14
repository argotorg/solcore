// MyToken — the OpenZeppelin Wizard token (ERC20 + Pausable + Ownable + Permit +
// Votes) in Core Solidity WITHOUT inheritance, with DISTRIBUTED storage.

import * from std;
import * from std.dispatch;
import {caller as opCaller, address as opAddress, chainid as opChainid, timestamp as opTimestamp} from std.opcodes;

import * from store;
import * from ownable;
import * from pausable;
import * from erc20core;
import * from votes;
import * from permit;

function caller() returns (address) { 
  return address(opCaller()); 
}

function selfAddress() returns (address) { 
  return address(opAddress()); 
}

function chainId() returns (uint256) { 
  return uint256(opChainid()); 
}

function nowTime() returns (uint256) { 
  return uint256(opTimestamp()); 
}

// The ERC20 `_update` seam, composing Pausable + core ledger + Votes.
function tokenUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasPaused, self: HasLedger, self: HasSupply, self: HasVotes {
  whenNotPaused(s);
  coreUpdate(s, from, to, value);
  votesOnTransfer(s, from, to, value);
}

contract MyToken {
  // No fields: the whole state lives in the capability modules' namespaces.

  constructor(initialOwner : address) {
    HasOwner.setOwner(appStore(), initialOwner);
  }

  // --- ERC20 metadata ---
  function name() public returns (memory<string>) { 
    return "MyToken"; 
  }
  
  function symbol() public returns (memory<string>) { 
    return "MTK"; 
  }
  
  function decimals() public returns (uint256) { 
    return uint256(18); 
  }

  // --- ERC20 views ---
  function totalSupply() public returns (uint256) { 
    return HasSupply.totalSupply(appStore()); 
  }
  
  function balanceOf(account : address) public returns (uint256) { 
    return HasLedger.balanceOf(appStore(), account); 
  }
  
  function allowance(o : address, spender : address) public returns (uint256) {
    return HasAllowance.allowanceOf(appStore(), o, spender);
  }

  // --- ERC20 actions (all movements flow through tokenUpdate) ---
  function transfer(to : address, value : uint256) public returns (bool) {
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    tokenUpdate(appStore(), caller(), to, value);
    return true;
  }
  function transferFrom(from : address, to : address, value : uint256) public returns (bool) {
    require(from != address(0), Error(0x96c6fd1e));          // ERC20InvalidSender
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    spendAllowance(appStore(), from, caller(), value);
    tokenUpdate(appStore(), from, to, value);
    return true;
  }
  function approve(spender : address, value : uint256) public returns (bool) {
    approveVal(appStore(), caller(), spender, value);
    return true;
  }

  // Added for testability (the Wizard omitted Mintable): owner-gated mint.
  function mint(to : address, value : uint256) public returns (()) {
    requireOwner(appStore(), caller());
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    tokenUpdate(appStore(), address(0), to, value);
  }

  // --- Ownable ---
  function owner() public returns (address) { 
    return HasOwner.getOwner(appStore()); 
  }
  
  function transferOwnership(newOwner : address) public returns (()) {
    requireOwner(appStore(), caller());
    HasOwner.setOwner(appStore(), newOwner);
  }

  // --- Pausable (owner-gated) ---
  function paused() public returns (uint256) { 
    return HasPaused.isPaused(appStore()); 
  }
  function pause() public returns (()) {
    requireOwner(appStore(), caller());
    HasPaused.setPaused(appStore(), uint256(1));
  }
  function unpause() public returns (()) {
    requireOwner(appStore(), caller());
    HasPaused.setPaused(appStore(), uint256(0));
  }

  // --- Votes (current voting power) ---
  function delegate(delegatee : address) public returns (()) {
    delegateTo(appStore(), caller(), delegatee);
  }
  function getVotes(account : address) public returns (uint256) {
    return HasVotes.votesOf(appStore(), account);
  }

  // --- Permit (EIP-2612) ---
  function nonces(o : address) public returns (uint256) { 
    return HasNonces.nonceOf(appStore(), o); 
  }
  function DOMAIN_SEPARATOR() public returns (bytes32) {
    return permitDomainSeparator(selfAddress(), chainId());
  }
  function permit(o : address, spender : address, value : uint256, deadline : uint256,
                  v : uint256, r : bytes32, s : bytes32) public returns (()) {
    runPermit(appStore(), o, spender, value, deadline, nowTime(), chainId(), selfAddress(), v, r, s);
  }
}
