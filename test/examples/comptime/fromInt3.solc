// import std.{Num,Add,Sub,Eq,Ord,Bounded,Typedef,le};
import std;

trait Int<i> {
  function fromWord(x:word) returns (comptime i); // meaning result is comptime whenever arg is

  function toWord(x:i) returns (comptime word);
}

impl Int<uint256> {
  function fromWord(x:word) returns (uint256) { return Typedef.abs(x); }
  function toWord(y:uint256) returns (word) { return Typedef.rep(y); }
}

impl Mul<uint256> {
  function mul(x: uint256, y: uint256) returns (uint256) {
    return Int.fromWord(Mul.mul(Int.toWord(x), Int.toWord(y)));
  }
}
impl Int<word> {
  function fromWord(x:word) returns (word) { return x; }
  function toWord(y:word) returns (word) { return y; }
}

function bitAnd(x:word, y:word) returns (comptime word) {
  let res : word;
  assembly {
    res := and(x,y)
  }
  return res;
}
function fromLit<a>(x:word) returns (a)  where a: Num { return Num.fromWord(x); }

contract FromInt {
   function main() returns (uint256) {
      let k = fromLit(40);
      return k+2;
   }
}