function undefined<any>() returns (any) {
  assembly {
    revert(0,0)
  }
}

function useWord(w:word) returns (()) {}

contract Magic {
  function main() public returns (()) {
    useWord(undefined());
  }
}
