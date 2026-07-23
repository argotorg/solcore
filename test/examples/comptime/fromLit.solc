import std;

trait FromLit<a, b> {
    function fromLit(l:b) returns (a);
}

function fromLit<a, b>(l:b) returns (a)  where a: FromLit<b> { return FromLit.fromLit(l); }

impl FromLit<word, word> {
    function fromLit(l:word) returns (word) { return l; }
}

impl FromLit<uint256, word> {
    function fromLit(l:word) returns (uint256) { return uint256(l); }
}

/*
// this does not define instance uint256:fromLit(uint256)
forall a.
default instance a:FromLit(a) {
    function fromLit(l:a) -> a { l }
}
*/
impl Mul<uint256> {
  function mul(a:uint256, b:uint256) returns (uint256) { return uint256(Mul.mul(Typedef.rep(a),Typedef.rep(b))); }
}

function main() returns (uint256) {
  let a : uint256 = fromLit(1);
  let comptime b :  uint256 = fromLit(2 + 2); // CTE
  let c : uint256 = fromLit(3) + fromLit(3); // RTE
  let comptime d :  word = fromLit(keccakLit("foo"+"bar")); // CTE

  return b*b - fromLit(4)*a*c + fromLit(d); // RTE in RTC
}