data Bool = False | True;

function test(x : Bool, y : Bool) -> Bool {
  match x, y {
  | Bool.True, z  => return z;
  | w, Bool.True  => return w;
  | a, b     => return b;
  }
}

contract FalseRedundantWarning {
  public function main() -> Bool {
    test(Bool.False, Bool.True)
  }
}
