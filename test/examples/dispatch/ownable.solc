import std.{*};
import std.dispatch.{*};

// caller() is not in the std library yet,
// so every contract must define its own

function caller() -> address {
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
  public function getOwner() -> address {
    return owner;
  }

  public function changeOwner(newOwner : address) -> () {
    require(caller() == owner, Error(0x12b0c500)); // OwnableUnauthorizedAccount()
    owner = newOwner;
  }
}
