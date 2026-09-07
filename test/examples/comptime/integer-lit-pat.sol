// Integer literal patterns against word and integer scrutinees.

import {Add} from std;

function classify_word(comptime n : word) returns (comptime word) {
  match (n ) {
    case 0 { return 10;
    } case 1 { return 20;
    } default { return 0;
  } }
}

function classify_integer(comptime n : integer) returns (comptime integer) {
  match (n ) {
    case 0 { return integerAdd(n, 10);
    } case 1 { return integerAdd(n, 20);
    } default { return n;
  } }
}

contract PatternLit {
  function main() returns (word) {
    let comptime a :  word = classify_word(1);
    let comptime b :  integer = classify_integer(0);
    return Add.add(a, wordFromInteger(b));
  }
}
