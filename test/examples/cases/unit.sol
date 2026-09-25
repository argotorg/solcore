contract Unit {
function one (x : unit) public returns (word) {
  return 1;
}

function unitVal() public returns (unit) {
  return;
}

function unitMatch (x : unit) public returns (word) {
  match (x ) {
  case () { return 1;
  } }
}

function foo (x : word) public returns (unit) {
  return;
}

function main() public returns (word) {
  return unitMatch(foo(one(unitVal())));
}
}

trait Def<a> {
  function def () returns (a) ;
}

impl Def<unit> {
  function def() returns (unit) {
    return;
  }
}
