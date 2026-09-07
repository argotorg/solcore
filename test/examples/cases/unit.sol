contract Unit {
function one (x : ()) public returns (word) {
  return 1;
}

function unitVal() public returns (()) {
  return;
}

function unitMatch (x : ()) public returns (word) {
  match (x ) {
  case () { return 1;
  } }
}

function foo (x : word) public returns (()) {
  return;
}

function main() public returns (word) {
  return unitMatch(foo(one(unitVal())));
}
}

trait Def<a> {
  function def () returns (a) ;
}

impl Def<()> {
  function def() returns (()) {
    return;
  }
}
