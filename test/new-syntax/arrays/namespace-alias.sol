import * as runtime from std;

contract ArrayNamespaceAlias {
  function main() returns (word) {
    let values: runtime.memory<runtime.DynArray<runtime.uint256>> = [10, 20];
    return 7;
  }
}
