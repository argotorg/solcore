import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

trait Clone<a> {
  function clone(x : a) returns (a);
}

impl Clone<word> {
  function clone(x : word) returns (word) { return x; }
}

#[derive(Clone)]
enum Box { Box(word) }

function cloneBox(x : Box) returns (Box) {
  return Clone.clone(x);
}
