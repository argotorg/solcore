function sum10() returns (word) {
  let s : word = 0;
  for (let i = 1; i <= 10; i = i + 1) {
    s = s + i;
  }
  return s;
}
