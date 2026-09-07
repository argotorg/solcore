import {memory, DynArray, uint256, Typedef, ridx, arrayLitNew as allocate, arrayLitInit as initialize} from std;

contract ArrayHelperAlias {
  function main() returns (word) {
    let values: memory<DynArray<uint256>> = [10, 20];
    let zero: uint256 = 0;
    return Typedef.rep(values[zero]);
  }
}
