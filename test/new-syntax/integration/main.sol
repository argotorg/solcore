import * from std;
import * from std.dispatch;

contract SyntaxIntegration {
  type Count = word;

  function run() public returns (uint256) {
    let constant: comptime<word> = computed(4);
    return uint256(arithmetic() + arrayElement() + nestedArrayElement() + proxy() + constant);
  }

  function arithmetic() returns (Count) {
    let value: Count = 12;
    value *= 3;
    value /= 2;
    value ~=;
    ~value
  }

  function arrayElement() returns (word) {
    let values: memory<DynArray<uint256>> = [10, 20, 30];
    let zero: uint256 = 0;
    let first: uint256 = values[zero];
    return Typedef.rep(first);
  }

  function nestedArrayElement() returns (word) {
    let emptyValues: memory<DynArray<uint256>> = [];
    let nested: memory<DynArray<memory<DynArray<uint256>>>> = [[1], [2, 3]];
    let one: uint256 = 1;
    let zero: uint256 = 0;
    let inner: memory<DynArray<uint256>> = nested[one];
    let value: uint256 = inner[zero];
    return Typedef.rep(value);
  }

  function lookup(index: uint256) public returns (uint256) {
    let values: memory<DynArray<uint256>> = [10, 20, 30];
    return values[index];
  }

  function lookupEmpty(index: uint256) public returns (uint256) {
    let values: memory<DynArray<uint256>> = [];
    return values[index];
  }

  function evaluationOrder() public returns (uint256) {
    let counter: word = allocate_zeroed_memory(32);
    let values: memory<DynArray<uint256>> = [next(counter), next(counter)];
    let zero: uint256 = 0;
    let one: uint256 = 1;
    let first: uint256 = values[zero];
    let second: uint256 = values[one];
    let count: word;
    assembly { count := mload(counter) }
    return uint256(count * 100 + Typedef.rep(first) * 10 + Typedef.rep(second));
  }

  function next(counter: word) returns (uint256) {
    let value: word;
    assembly {
      value := add(mload(counter), 1)
      mstore(counter, value)
    }
    return uint256(value);
  }

  function proxy() returns (word) {
    let witness: @word = @word;
    return 42;
  }

  function computed(comptime input: word) returns (comptime<word>) {
    let result: comptime<word> = input + 1;
    return result;
  }
}
