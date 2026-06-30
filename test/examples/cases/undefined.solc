forall any.function undefined() -> any {
  assembly {
    revert(0,0)
  }
}

function useWord(w:word) -> () {}

contract Magic {
  public function main() -> () {
    useWord(undefined());
  }
}
