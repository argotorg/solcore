import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract Simple {
  myval : word ;

  function getVal () public returns (word) {
    return myval ;
  }

  function main () public returns (word) {
    return getVal();
  }
}
