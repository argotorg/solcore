contract ComptimeSyntax {

  function f(comptime x : word) returns (comptime word) {
    return x;
  }

  function g() returns (word) {
    let comptime y :  word = f(42);
    return y;
  }

  function main() returns (word) {
    return g();
  }
}
