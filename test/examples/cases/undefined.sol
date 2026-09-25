function undefined<any>() returns (any) {
  assembly {
    revert(0,0)
  }
}

function useWord(w:word) returns (unit) {}

contract Magic {
  function main() public returns (unit) {
    useWord(undefined());
  }
}
