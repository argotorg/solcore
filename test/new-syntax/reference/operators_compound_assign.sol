function operators(x: word, y: word, z: word) returns (word) {
  let acc = x % y;
  acc = (acc & y) | (x ^ z);
  acc += x;
  acc -= y;
  acc ^= z;
  acc &= x;
  acc |= y;
  acc %= z;
  return acc;
}
