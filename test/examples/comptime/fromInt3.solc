// import std.{Num,Add,Sub,Eq,Ord,Bounded,Typedef,le};
import std;

forall i.
class i : Int {
  function fromWord(x:word) -> comptime i; // meaning result is comptime whenever arg is

  function toWord(x:i) -> comptime word;
}

instance  uint256 : Int {
  function fromWord(x:word) -> uint256 { Typedef.abs(x) }
  function toWord(y:uint256) -> word { Typedef.rep(y) }
}

instance uint256 : Mul {
  function mul(x: uint256, y: uint256) -> uint256 {
    Int.fromWord(Mul.mul(Int.toWord(x), Int.toWord(y)))
  }
}
instance word : Int {
  function fromWord(x:word) -> word { x }
  function toWord(y:word) -> word { y }
}

function bitAnd(x:word, y:word) -> comptime word {
  let res : word;
  assembly {
    res := and(x,y)
  }
  return res;
}
forall a. a: Num =>
function fromLit(x:word) -> a { Num.fromWord(x) }

contract FromInt {
   function main() -> uint256 {
      let k = fromLit(40);
      return k+2;
   }
}