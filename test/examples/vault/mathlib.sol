// Style 2: a "library" of checked arithmetic, as free functions.
import * from std;

export { addChecked, subChecked };

function addChecked(a : uint256, b : uint256) returns (uint256) {
  let c : uint256 = Num.add(a, b);
  require(c >= a, Error(0x4e487b71));   // overflow
  return c;
}

function subChecked(a : uint256, b : uint256) returns (uint256) {
  require(a >= b, Error(0x4e487b71));   // underflow (insufficient balance / supply)
  return Num.sub(a, b);
}
