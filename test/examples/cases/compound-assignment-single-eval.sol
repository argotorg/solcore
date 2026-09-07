import {*} from std;

contract CompoundAssignmentSingleEval {
  values: mapping(word => word);
  indexCalls: word;

  function index() returns (word) {
    indexCalls += 1;
    return 0;
  }

  function bump() {
    values[index()] += 1;
  }
}
