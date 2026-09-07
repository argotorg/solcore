function main(foo: (word, word)) returns (word) {
  let res: word;
  match (foo) {
case (v0, v1) {
{
    let x: word = v1;
    res = x;
  }
}
}
  return res;
}
