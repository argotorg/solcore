
function fromWord<a>(x: word) returns (a) {
      let result : a;
      assembly { result := x } 
      return result;
  }

contract Unsafe {
  function main() public {
    fromWord(7) as ();
    return 42;
  }
}
