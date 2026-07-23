import {*} from std;
import {*} from std.dispatch;

// caller() is not in the std library yet,
// so every contract must define its own

function caller() returns (address) {
  let res: word;
  assembly {
     res := caller()
  }
  return address(res);
}

contract Ownable {
  owner : address;

  constructor() {
    owner = caller();
  }

  // named getOwner() instead of owner() to avoid collision with the field name
  function getOwner() public returns (address) {
    return owner;
  }

  function changeOwner(newOwner : address) public returns (()) {
    require(caller() == owner, Error(0x12b0c500)); // OwnableUnauthorizedAccount()
    owner = newOwner;
  }
}
