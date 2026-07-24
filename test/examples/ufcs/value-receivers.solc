import {*} from std;

trait ReceiverMethod<a> {
  function project(value: a, salt: word) returns (word);
}

impl ReceiverMethod<word> {
  function project(value: word, salt: word) returns (word) {
    return value + salt;
  }
}

function makeValue(value: word) returns (word) {
  return value;
}

contract ValueReceivers {
  values: word[];

  constructor() {}

  function fromParam(value: word) public returns (word) {
    return value.project(1);
  }

  function fromLocal(value: word) public returns (word) {
    let local: word = value;
    return local.project(2);
  }

  function fromIndex(index: uint256) public returns (word) {
    return values[index].project(3);
  }

  function fromComputed(value: word) public returns (word) {
    return (makeValue(value) + 4).project(5);
  }
}
