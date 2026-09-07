import {lt,Ord,Add,Sub,Bounded,Num,Eq,Typedef} from std;
contract BreakTest {
  function main() public returns (word) {
      let result : word = 0;
      for (let i : word = 0; i < 10; i = i + 1) {
          if (i == 5) {
              break;
          } else {}
          result = result + 1;
      }
      return result;
  }
}
