import std.{*};
import std.dispatch.{*};

// Storage support for a `memory(bytes)` contract field: assigning to the
// field copies the byte array into storage, reading it back loads it into
// fresh memory. Exercises StorageSize / CanStore for memory(bytes).
contract C {
  content: bytes;

  public function set(value: memory(bytes)) -> () {
    content = value;
  }

  public function get() -> memory(bytes) {
    return content;
  }
}
