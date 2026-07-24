import std.{*};
import std.dispatch.{*};

// Nested storage arrays and aliasing, on the EVM.
//
// `grid[i]` yields the inner array's storage handle, so it can be pushed to and
// indexed again. Each inner array lives at its own slot keccak256(outer) + i, and
// its elements at keccak256(that slot) + j.
//
// Binding an array field to a local is an alias (Solidity's `T[] storage p`), not
// a copy: mutating through the local must be visible through the field.
contract NestedArray {
  grid : array(array(uint256));
  flat : array(uint256);

  constructor() {}

  public function growOuter(n : uint256) -> () {
    Array.setLength(grid, n);
  }

  // grid[i].push(v) -- the inner handle comes straight out of the index
  public function pushInner(i : uint256, v : uint256) -> () {
    ArrayPush.push(grid[i], v);
  }

  public function innerLen(i : uint256) -> uint256 {
    return Length.length(grid[i]);
  }

  public function get2(i : uint256, j : uint256) -> uint256 {
    return grid[i][j];
  }

  public function set2(i : uint256, j : uint256, v : uint256) -> () {
    grid[i][j] = v;
  }

  // Mutate `flat` through a local alias; the field must observe it.
  public function aliasPush(v : uint256) -> () {
    let p : storage(array(uint256)) = flat;
    ArrayPush.push(p, v);
  }

  public function aliasSet(i : uint256, v : uint256) -> () {
    let p : storage(array(uint256)) = flat;
    p[i] = v;
  }

  public function flatLen() -> uint256 {
    return Length.length(flat);
  }

  public function getFlat(i : uint256) -> uint256 {
    return flat[i];
  }
}
