// After an assembly block writes to a pattern variable, subsequent code in the
// same match arm should read the written value (not the original tuple component).
// Runtime correctness of the write->read depends on ecSubst being updated after
// the assembly block (EmitHull.hs: emitStmt MastAsm, modify ecSubst).
contract C {
  function main() -> word {
    let res : word;
    let foo : (word,word) = (0, 0);
    match foo {
      | (v0, v1) => {
        assembly { v1 := 42 }
        let x : word = v1;
        assembly { res := x }
      }
    }
    return res;
  }
}
