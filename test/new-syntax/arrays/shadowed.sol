import * from std hiding {arrayLitNew, arrayLitInit};

function arrayLitNew() returns (word) { return 99; }
function arrayLitInit() returns (word) { return 99; }

contract ArrayShadowedHelpers {
  function main() returns (word) {
    let arrayLitNew: word = 88;
    let arrayLitInit: word = 88;
    let values: memory<DynArray<uint256>> = [10, 20];
    let zero: uint256 = 0;
    return Typedef.rep(values[zero]);
  }
}
