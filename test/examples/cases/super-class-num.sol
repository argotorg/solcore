enum Bool { False, True }

function fromBool(b:Bool) returns (word) {
 match (b ) {
   case Bool.False { return 0;
   } case Bool.True { return 1;
 } }
}

function toBool(x: word) returns (Bool) {
  match (x ) {
    case 0 { return Bool.False;
    } default { return Bool.True;
  } }
}

trait Eq<a> {
  function eq(x:a, y:a) returns (Bool);
}

impl Eq<word> {
  function eq(x:word, y:word) returns (Bool) {
    let res : word;
    assembly {
       res := eq(x, y)
    }
    return toBool(res);
  }
}

function not (b : Bool) returns (Bool) {
  match (b ) {
  case Bool.True { return Bool.False ;
  } case Bool.False { return Bool.True ;
  } }
}

function ne<a>(x : a, y : a) returns (Bool)  where a: Eq {
  return not(Eq.eq(x,y));
}

trait Num<a> where a: Eq {
  function toWord(x:a) returns (word);
  function fromWord(x:word) returns (a);
}

impl Num<word> {
  function toWord(x:word) returns (word) { return x; }
  function fromWord(x:word) returns (word) { return x; }
}


enum uint { uint(word) }

impl Eq<uint> {
  function eq(x:uint, y:uint) returns (Bool) { return Eq.eq(Num.toWord(x), Num.toWord(y)); }
}


impl Num<uint> {
  function toWord(x:uint) returns (word)
  {
          match (x ) {
            case uint(y) { return y;
        } }
  }
  function fromWord(x:word) returns (uint) { return uint(x); }
}
