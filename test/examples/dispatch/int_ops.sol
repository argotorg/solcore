// Exercises std.signed and std.unsigned semantics. Each function returns the raw
// two's-complement word (via Typedef.rep) so the harness can check exact values.
import * from std;
import * from std.signed;
import * from std.unsigned;
import * from std.dispatch;

contract IntOps {
  acc : int64;

  constructor() {}

  // int8 param + int8 return: ABIDecode reduces the param, ABIEncode returns it.
  // -8 negated -> 8
  function negate8(x : int8) public returns (int8) {
    return 0 - x;
  }

  // int32 params + return: 2000000000 + 2000000000 wraps int32
  // (4000000000 - 2^32 = -294967296)
  function addI32(a : int32, b : int32) public returns (int32) {
    return a + b;
  }

  // int64 storage field via CanStore/StorageType.
  function setAcc(v : int64) public returns (int64) {
    acc = v;
    return acc;
  }

  function getAcc() public returns (int64) {
    return acc;
  }

  // signed multiply: (-5) * 5 = -25
  function mulNeg() public returns (uint256) {
    let d : int256 = 5;
    return uint256(Typedef.rep((0 - d) * d));
  }

  // int8 wraps: 100 + 100 = 200 -> -56 (sign-extended)
  function addWrap8() public returns (uint256) {
    let a : int8 = 100;
    return uint256(Typedef.rep(a + a));
  }

  // signed compare: -5 < 5 -> true
  function ltSigned() public returns (uint256) {
    let d : int256 = 5;
    let e : int256 = 0 - d;
    if (e < d) { return uint256(1); } else { return uint256(0); }
  }

  // signed divide (truncates toward zero): -20 / 3 = -6
  function divSigned() public returns (uint256) {
    let m20 : int256 = 0 - 20;
    let three : int256 = 3;
    return uint256(Typedef.rep(m20 / three));
  }

  // uint8 wraps: 200 + 100 = 300 & 0xff = 44
  function addWrapU8() public returns (uint256) {
    let a : uint8 = 200;
    let b : uint8 = 100;
    return uint256(Typedef.rep(a + b));
  }
}
