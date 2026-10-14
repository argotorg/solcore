// MyToken — the OpenZeppelin Wizard token (ERC20 + Pausable + Ownable + Permit +
// Votes) in Core Solidity WITHOUT inheritance, HOOK-based architecture.
import * from std;
import * from std.dispatch;
import {address as opAddress, chainid as opChainid, timestamp as opTimestamp} from std.opcodes;

import {sender} from context;
import * from option;
import {Stacked} from hooks;
import {balance, supply, apply} from erc20core;
import {allowanceOf, approveAllowance, spendAllowance} from allowance;
import {owner as storedOwner, initOwner, requireOwner, setOwner} from ownable;
import {isPaused, setPaused, Pausable} from pausable;
import {votesOf, delegateTo, Votes} from votes;
import {nonceOf, runPermit, permitDomainSeparator} from permit;

function selfAddress() returns (address) {
  return address(opAddress());
}

function chainId() returns (uint256) {
  return uint256(opChainid());
}

function nowTime() returns (uint256) {
  return uint256(opTimestamp());
}

// The single choke point: the hook chain is declared once here. Adding a
// feature means writing a TransferHook and adding it to this Stacked.
function update(from : Option<address>, to : Option<address>, amount : uint256) returns (()) {
  apply(Stacked(Pausable.Pausable, Votes.Votes), from, to, amount);
}

contract MyToken {
  // No fields: the whole state lives in the capability modules' namespaces.

  constructor(initialOwner : address) {
    initOwner(initialOwner);
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
    return supply();
  }

  function balanceOf(account : address) public returns (uint256) {
    return balance(account);
  }

  function allowance(o : address, spender : address) public returns (uint256) {
    return allowanceOf(o, spender);
  }

  // --- ERC20 actions (all movements flow through update) ---
  function transfer(to : address, value : uint256) public returns (bool) {
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    update(Option.Some(sender()), Option.Some(to), value);
    return true;
  }

  function transferFrom(from : address, to : address, value : uint256) public returns (bool) {
    require(from != address(0), Error(0x96c6fd1e));          // ERC20InvalidSender
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    spendAllowance(from, sender(), value);
    update(Option.Some(from), Option.Some(to), value);
    return true;
  }

  function approve(spender : address, value : uint256) public returns (bool) {
    approveAllowance(sender(), spender, value);
    return true;
  }

  // Added for testability (the Wizard omitted Mintable): owner-gated mint.
  function mint(to : address, value : uint256) public returns (()) {
    requireOwner();
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    update(Option.None, Option.Some(to), value);
  }

  // --- Ownable ---
  function owner() public returns (address) {
    return storedOwner();
  }

  function transferOwnership(newOwner : address) public returns (()) {
    requireOwner();
    setOwner(newOwner);
  }

  // --- Pausable (owner-gated) ---
  function paused() public returns (uint256) {
    return isPaused();
  }

  function pause() public returns (()) {
    requireOwner();
    setPaused(uint256(1));
  }

  function unpause() public returns (()) {
    requireOwner();
    setPaused(uint256(0));
  }

  // --- Votes (current voting power) ---
  function delegate(delegatee : address) public returns (()) {
    delegateTo(sender(), delegatee);
  }

  function getVotes(account : address) public returns (uint256) {
    return votesOf(account);
  }

  // --- Permit (EIP-2612) ---
  function nonces(o : address) public returns (uint256) {
    return nonceOf(o);
  }

  function DOMAIN_SEPARATOR() public returns (bytes32) {
    return permitDomainSeparator(selfAddress(), chainId());
  }

  function permit(o : address, spender : address, value : uint256, deadline : uint256,
                  v : uint256, r : bytes32, s : bytes32) public returns (()) {
    runPermit(o, spender, value, deadline, nowTime(), chainId(), selfAddress(), v, r, s);
  }
}
