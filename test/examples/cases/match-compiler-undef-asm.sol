enum Foo<a> { Foo(word) }

function read<a>(x : Foo<a>) returns (word) {
  let res : word;
  match (x) {
  case Foo(w) {
    assembly {
      res := w
    }
  } }
  return res;
}

contract Bla {

  function main () public returns (word) {
    return read(Foo(42));
  }
}
