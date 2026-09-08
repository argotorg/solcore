import foo;
import * as Bar from foo.bar;

function main() returns (word) {
  let x: word = foo.base();
  let y: word = Bar.value();
  return y;
}
