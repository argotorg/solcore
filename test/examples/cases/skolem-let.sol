
function fromWord<a>(x: word) returns (a) {
      let result : a;
      assembly { result := x } 
      return result;
  }

contract Unsafe {
  function main() public {
    let syntaxValue1: () = fromWord(7);
    syntaxValue1;
    return 42;
  }
}
