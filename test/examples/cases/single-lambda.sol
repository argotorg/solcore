function foo () returns (function(word) returns (bool)) {
  return lambda (x:word) -> bool { return true; };
}
