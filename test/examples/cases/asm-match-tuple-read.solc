contract C {
  function main() -> word {
    let res : word;
    let foo : (word,word) = (1, 42);
    match foo {
      | (v0, v1) => assembly { res := v1 }
    }
    return res;
  }
}
