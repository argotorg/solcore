contract ComptimeNamedReturn {
  function staged(comptime x: word) returns (comptime<word>) {
    let result: comptime<word> = x;
    return result;
  }

  function main() returns (word) {
    return staged(42);
  }
}
