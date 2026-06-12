type W = word;

forall i.
class i : FromWord {
  function fromWord(x:word) ->  i;
}

instance word : FromWord {
  function fromWord(x:word) -> word { x }
}

contract C {

  public function main () -> W {
    let r : W = FromWord.fromWord(42);
    return r;
  }
}
