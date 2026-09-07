import {*} from std;

contract TupleRet {
  constructor() {}

  function pair() returns ((uint256, uint256)) {
    return (uint256(7), uint256(11));
  }
}
