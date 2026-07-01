// Bare integer literals at uint256-typed sites use `instance uint256 : Int`.
// The instance's fromInteger wraps `wordFromInteger`, so an out-of-range
// literal is truncated mod 2^256, matching the `word` site behaviour.
import std.{*};

contract Uint256Lit {
  function main() -> word {
    let a : uint256 = 3;
    // 2^256 + 5 must truncate to 5.
    let b : uint256 = 0x10000000000000000000000000000000000000000000000000000000000000005;
    return Typedef.rep(a) + Typedef.rep(b);
  }
}
