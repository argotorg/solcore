trait Fallback<a> {
  function tag(x:a) returns (word);
}

default impl<a> Fallback<a> {
  function tag(x:a) returns (word) {
    return 7;
  }
}

function main() returns (word) {
  return Fallback.tag(0 as word);
}
