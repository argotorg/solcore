import {*} from std;
import {*} from std.dispatch;

// Storage support for a `memory(bytes)` contract field: assigning to the
// field copies the byte array into storage, reading it back loads it into
// fresh memory. Exercises StorageSize / CanStore for memory(bytes).
contract C {
  content: bytes;

  function set(value: bytes memory) public returns (()) {
    content = value;
  }

  function get() public returns (bytes memory) {
    return content;
  }
}
