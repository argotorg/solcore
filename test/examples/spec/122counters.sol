// test multiple contract fields
import {*} from std;
// import StorageLib;


contract Counter {
  counter1 : word;
  counter2 : uint256;
  counter3 : word;
  function main() public returns (word) {
    counter1 += 1;
    counter3 += 2;
    return counter1 + counter3;
  }
}
