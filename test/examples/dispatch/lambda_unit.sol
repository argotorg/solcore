// Illustrates the two surface-syntax changes:
//   * the lambda keyword is spelled out (`lambda`, not the old `lam`);
//   * the unit type is written `unit`; the reviewer's confusing
//     `lambda () -> ()` is now written `lambda () -> unit` (here the empty
//     parens are the parameter list and `unit` is the result type).
import * from std;
import * from std.dispatch;

contract LambdaUnit {
  constructor() {}

  function run(x : uint256) public returns (uint256) {
    // A nullary lambda whose result type is annotated with the `unit` surface
    // name, invoked for its effect.
    let noop = lambda () -> unit { return; };
    noop();

    // A value-producing lambda, applied immediately.
    return (lambda (v : uint256) -> uint256 { return v + v; })(x);
  }
}
