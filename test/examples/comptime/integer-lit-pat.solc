// Integer literal patterns against word and integer scrutinees.

import std.{Add};

function classify_word(comptime n : word) -> comptime word {
  match n {
    | 0 => return 10;
    | 1 => return 20;
    | _ => return 0;
  }
}

function classify_integer(comptime n : integer) -> comptime integer {
  match n {
    | 0 => return integerAdd(n, 10);
    | 1 => return integerAdd(n, 20);
    | _ => return n;
  }
}

contract PatternLit {
  function main() -> word {
    let a : comptime word = classify_word(1);
    let b : comptime integer = classify_integer(0);
    return Add.add(a, wordFromInteger(b));
  }
}
