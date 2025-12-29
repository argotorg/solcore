function foo(length:word, pos:word) -> word {
  let ret: word;
  assembly {
	// ret := add(pos, mul(0x20, iszero(iszero(length))))
	ret := iszero(iszero(length))
  }
  return ret;
}
