
trait A<self> {
  function foo(p : self) returns (word);
}

trait B<self> {
  function foo(p : self) returns (word);
}

impl B<word> {
  function foo(x : word) returns (word) {
    return x;
  }
}

// error: Constraint for A not found in type of foo
impl A<word> {
  function foo(x : word) returns (word) {
    return x;
  }
}
