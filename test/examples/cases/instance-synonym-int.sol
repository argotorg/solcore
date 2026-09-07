alias W = word;

trait FromWord<i> {
  function fromWord(x:word) returns (i);
}

impl FromWord<word> {
  function fromWord(x:word) returns (word) { return x; }
}

contract C {

  function main () public returns (W) {
    let r : W = FromWord.fromWord(42);
    return r;
  }
}
