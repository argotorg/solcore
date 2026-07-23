contract ComptimeNamedReturn {
  function staged(comptime x: word) returns (comptime result: word) {
    result = x;
    return;
  }

  function main() returns (word) {
    return staged(42);
  }
}
