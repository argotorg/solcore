// VaultToken — the Classic Solidity example from valt-verify (Ownable + Pausable +
// ERC20Base + flash-mint), in Core Solidity WITHOUT inheritance, distributed storage.
// (VaultTokenFactory / CREATE2 omitted: needs multi-contract compilation + creationCode.)

import * from std;
import * from std.dispatch;
import {caller as opCaller, address as opAddress} from std.opcodes;

import * from store;
import * from mathlib;
import * from ownable;
import * from pausable;
import * from erc20base;
import * from flashmint;

function caller() returns (address) { return address(opCaller()); }
function selfAddress() returns (address) { return address(opAddress()); }

// The `_update` seam: whenNotPaused then the ERC20Base ledger movement
// (the Solidity `_update` override plus its `super._update`, made explicit).
function vaultUpdate<self>(s : self, from : address, to : address, value : uint256) returns (())
  where self: HasPaused, self: HasLedger, self: HasSupply {
  whenNotPaused(s);
  coreUpdate(s, from, to, value);
}

contract VaultToken {
  // No fields: the state lives in the capability modules' namespaces.

  constructor(initialSupply : uint256) {
    HasOwner.setOwner(appStore(), caller());               // Ownable(): owner = deployer
    vaultUpdate(appStore(), address(0), caller(), initialSupply); // mint to deployer
  }

  // --- Ownable ---
  function owner() public returns (address) { return HasOwner.getOwner(appStore()); }
  function transferOwnership(to : address) public returns (()) {
    requireOwner(appStore(), caller());
    HasOwner.setOwner(appStore(), to);
  }

  // --- Pausable (owner-gated) ---
  function paused() public returns (uint256) { return HasPaused.isPaused(appStore()); }
  function setPaused(p : uint256) public returns (()) {
    requireOwner(appStore(), caller());
    HasPaused.setPaused(appStore(), p);
  }

  // --- ERC20 views ---
  function totalSupply() public returns (uint256) { return HasSupply.totalSupply(appStore()); }
  function balanceOf(account : address) public returns (uint256) { return HasLedger.balanceOf(appStore(), account); }

  // --- ERC20 actions (through the seam) ---
  function transfer(to : address, amount : uint256) public returns (bool) {
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    vaultUpdate(appStore(), caller(), to, amount);
    return true;
  }
  function mint(to : address, amount : uint256) public returns (()) {
    requireOwner(appStore(), caller());
    require(to != address(0), Error(0xec442f05));            // ERC20InvalidReceiver
    vaultUpdate(appStore(), address(0), to, amount);
  }

  // --- Style 3: flash-mint with an interface callback ---
  function flashMint(borrower : address, amount : uint256) public returns (()) {
    vaultUpdate(appStore(), address(0), borrower, amount);   // ephemeral mint
    callBorrower(borrower, caller(), amount);                // callback + magic check
    vaultUpdate(appStore(), borrower, address(0), amount);   // burn back
  }

  // The vault plays its own flash borrower in the self-referential execution test.
  function onFlashMint(initiator : address, amount : uint256) public returns (bytes32) {
    return bytes32(flashOk());
  }

  // Convenience entry point for the self-borrow test.
  function flashMintSelf(amount : uint256) public returns (()) {
    flashMint(selfAddress(), amount);
  }
}
