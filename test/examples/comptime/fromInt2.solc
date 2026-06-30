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
     let a : uint256 = fromLit(1);
     let b : comptime uint256 = fromLit((2 + 2)); // CTE
     let c : uint256 = fromLit(3) + fromLit(3); // RTE
     // let d : comptime word = fromLit(bitAnd(0xff,keccakLit("foo"+"bar"))); // CTE
    let d : comptime word = fromLit(bitAnd(0xff,keccakLit("foo"+"bar"))); // CTE

     let k = fromLit(40);
     return k+2;
     // return b*b + fromLit(4)*a*c + fromLit(d);
   }
}