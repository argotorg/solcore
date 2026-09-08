import {memory, DynArray, uint256, Typedef, ridx} from std;

contract ArraySelectiveImport {
  function main() returns (word) {
    let values: memory<DynArray<uint256>> = [10, 20];
    let zero: uint256 = 0;
    return Typedef.rep(values[zero]);
  }
}
