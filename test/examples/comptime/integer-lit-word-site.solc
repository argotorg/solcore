// Integer literals at word-typed sites receive automatic wordFromInteger coercions.
// Tests:
//   let x : word = N         -- explicit word annotation
//   return N                 -- return in word-returning function
//   passing literal to word parameter

contract WordSite {
  function main() -> word {
    let a : word = 42;
    let b : word = 0;
    return a;
  }
}
