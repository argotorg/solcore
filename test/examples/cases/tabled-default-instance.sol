trait Fallback<a> {
  function tag(x:a) returns (word);
}

default impl<a> Fallback<a> {
  function tag(x:a) returns (word) {
    return 7;
  }
}

function main() returns (word) {
  let syntaxValue1: word = 0;
  return Fallback.tag(syntaxValue1);
}
