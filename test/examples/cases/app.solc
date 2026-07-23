function app<a, b, c> (f : c, x : a) returns (b)  where c: invokable<a, b> {
  return invokable.invoke(f, x);
}

enum t_id { t_id }

impl invokable<t_id, word, word> {
  function invoke(self : t_id, x : word) returns (word) {
    return x;
  }
}

function foo() returns (word) {
  return app(t_id, 0);
}

contract C {
  function main () public returns (word) {
    return foo();
  }
}
