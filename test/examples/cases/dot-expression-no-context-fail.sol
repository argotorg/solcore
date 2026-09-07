enum Option { None, Some(word) }

function bad() returns (Option) {
  let x = .Some(1);
  return x;
}
