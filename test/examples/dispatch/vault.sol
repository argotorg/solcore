// Vault: a contract that INTERACTS WITH ANOTHER CONTRACT (an ERC20 token).
//
// A depositor first `approve`s the vault on the token, then calls `deposit`,
// which makes the vault call the token's `transferFrom(depositor, vault, amount)`
// to pull the tokens in and credit an internal ledger. `withdraw` does the
// reverse: it debits the ledger and calls the token's `transfer(depositor,
// amount)` to push the tokens back.
//
// Core Solidity has no typed external interface (no `IERC20(token).transfer(..)`).
// A cross-contract call is made by hand: build `selector ++ abi_encode(args)`
// with `concat`, then `raw_call(tokenAddress, 0, payload)`, and check the
// returned success flag. This is exactly what the classic-Solidity `IERC20`
// interface compiles down to.
import * from std;
import * from std.dispatch;
import {address as address_} from std.opcodes;

// msg.sender: the account that called this contract.
function caller() returns (address) {
  let res : word;
  assembly {
     res := caller()
  }
  return address(res);
}

// address(this): this contract's own address.
function self() returns (address) {
  return address(address_());
}

contract Vault {
  token : address;                       // the ERC20 this vault holds
  ledger : mapping(address => uint256);  // internal deposit balances

  constructor(token_ : address) {
    token = token_;
  }

  // token.transferFrom(src, dst, amt) via a hand-built external call.
  // Selector of "transferFrom(address,address,uint256)" = 0x23b872dd.
  function callTransferFrom(src : address, dst : address, amt : uint256) returns (bool) {
    let sel : bytes32 = bytes32(0x23b872dd00000000000000000000000000000000000000000000000000000000);
    let payload = concat(truncate(to_bytes(sel), 4), abi_encode((src, dst, amt)));
    match (raw_call(token, uint256(0), payload) ) {
      case (ok, ret) { return ok;
    } }
  }

  // token.transfer(dst, amt) via a hand-built external call.
  // Selector of "transfer(address,uint256)" = 0xa9059cbb.
  function callTransfer(dst : address, amt : uint256) returns (bool) {
    let sel : bytes32 = bytes32(0xa9059cbb00000000000000000000000000000000000000000000000000000000);
    let payload = concat(truncate(to_bytes(sel), 4), abi_encode((dst, amt)));
    match (raw_call(token, uint256(0), payload) ) {
      case (ok, ret) { return ok;
    } }
  }

  // Pull `amount` tokens from the caller into the vault, crediting the ledger.
  // Requires the caller to have approved this vault on the token first.
  function deposit(amount : uint256) public returns (bool) {
    let from = caller();
    require(callTransferFrom(from, self(), amount), Error(0x90b8ec18)); // TransferFailed()
    ledger[from] = ledger[from] + amount;
    return true;
  }

  // Return `amount` tokens from the vault to the caller, debiting the ledger.
  function withdraw(amount : uint256) public returns (bool) {
    let to = caller();
    require(ledger[to] >= amount, Error(0xb4120f14)); // OutOfBounds()/insufficient
    ledger[to] = ledger[to] - amount;
    require(callTransfer(to, amount), Error(0x90b8ec18)); // TransferFailed()
    return true;
  }

  // The vault's internal record of how much `user` has deposited.
  function balanceOf(user : address) public returns (uint256) {
    return ledger[user];
  }

  // The token address this vault is bound to.
  function tokenAddress() public returns (address) {
    return token;
  }
}
