import * from std;
import * from std.dispatch;

contract Ownable {
  owner : address;

  constructor() {
    owner = msgSender();
  }

  // named getOwner() instead of owner() to avoid collision with the field name
  function getOwner() public returns (address) {
    return owner;
  }

  function changeOwner(newOwner : address) public returns (()) {
    require(msgSender() == owner, Error(0x12b0c500)); // OwnableUnauthorizedAccount()
    owner = newOwner;
  }
}
