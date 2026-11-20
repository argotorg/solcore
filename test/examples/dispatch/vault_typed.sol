// Vault using a TYPED interface for the external calls. `interface IERC20`
// desugars to a handle newtype + internal trait + external-call stubs, so the
// interface can be a typed field (`token : IERC20`) and `token.transferFrom(..)`
// is a typed cross-contract call that lowers to selector + abi_encode + raw_call.
import * from std;
import * from std.dispatch;
import {address as address_} from std.opcodes;

function caller() returns (address) {
  let r : word;
  assembly { r := caller() }
  return address(r);
}

function self() returns (address) {
  return address(address_());
}

interface IERC20 {
  function transfer(to : address, amount : uint256) returns (bool);
  function transferFrom(from : address, to : address, amount : uint256) returns (bool);
}

contract Vault {
  token : IERC20;                        // the interface handle, stored as a field
  ledger : mapping(address => uint256);

  constructor(token_ : address) {
    token = IERC20(token_);
  }

  function deposit(amount : uint256) public returns (bool) {
    require(token.transferFrom(caller(), self(), amount), Error(0x90b8ec18));
    ledger[caller()] = ledger[caller()] + amount;
    return true;
  }

  function withdraw(amount : uint256) public returns (bool) {
    require(ledger[caller()] >= amount, Error(0xb4120f14));
    ledger[caller()] = ledger[caller()] - amount;
    require(token.transfer(caller(), amount), Error(0x90b8ec18));
    return true;
  }

  function balanceOf(user : address) public returns (uint256) {
    return ledger[user];
  }
}
