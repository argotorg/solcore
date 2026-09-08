type comptime = word;

contract ComptimeModifier {
  function f(comptime x: word) returns (comptime<word>) {
    return x;
  }

  function identifier(x: comptime) returns (comptime) {
    let comptime : word = 1;
    let y : comptime<word> = f(comptime);
    return y;
  }
}
