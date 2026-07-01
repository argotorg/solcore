// Exercises integer primitives: wordToInteger, wordFromInteger, integerAdd, integerMul.
// Integer-typed lets are implicitly comptime; literals are polymorphic via FromInteger.
// Expected: main() folds to word literal 100.

contract IntegerBasic {
  function main() -> word {
    let x = 42;
    let y = integerAdd(x, 8);
    return wordFromInteger(integerMul(y, 2));
  }
}
