/* Handling numeric literals

Eventually we may want to have a comptime integer type (unlimited precision)
and literals desugar to `fromInteger(lit)`

Here we use a bit less ambitious approach: literals of type word and `fromWord` method
*/

import std;

type uint = uint256; // misleads instance solver

forall i.
class i : Int {
  function fromWord(x:word) -> comptime i; // meaning result is comptime whenever arg is

  function toWord(x:i) -> comptime word;
}


instance word : Int {
  function fromWord(x:word) -> comptime word { x }
  function toWord(x:word) -> comptime word { x }
}

instance uint : Int {
  function fromWord(x:word) -> comptime uint { uint256(x) }
  function toWord(x:uint) -> comptime word { Typedef.rep(x) }
}


// specialised for numbers
forall a b. a:Int, b:Int => function fromInt(x:a) -> b { Int.fromWord(Int.toWord(x)) }
forall a b. a:Int, b:Int => function staticInt(comptime x:a) -> comptime b { Int.fromWord(Int.toWord(x)) }

// limited usability
forall a b r. a:Typedef(r), b:Typedef(r) => function dynamic_cast(x:a) -> b { Typedef.abs(Typedef.rep(x):r) }
forall a b r. a:Typedef(r), b:Typedef(r) => function static_cast(comptime x:a) -> comptime b { Typedef.abs(Typedef.rep(x):r) }

// wider usability
forall a b r. a:Typedef(r), b:Typedef(r) =>
function dynamic_cast_via(p:@r, x:a) -> b { Typedef.abs(Typedef.rep(x):r) }

forall a b r. a:Typedef(r), b:Typedef(r) =>
function static_cast_via(comptime p:@r, comptime x:a) -> comptime b { Typedef.abs(Typedef.rep(x):r) }
// maybe: `comptime function static_cast_via` as equivalent notation

function notcomptime(x:word) -> word {
  let res : word;
  assembly {
    res := mload(0)
  }
  return res;
}

forall a. function id(x:a) -> comptime a { x }
function id_uint(x:uint) -> comptime uint { x }
contract FromWord {
  constructor() {}
  function f1(x : word) -> comptime word { x }
  function f2(x : uint) -> comptime uint { x }
  function g() -> uint {
    let y1 : comptime uint256 = static_cast( // cast on top level of comptime let
                                     f1(
				        static_cast(42) //cast a literal - could be fromWord/staticInt
				     ));

    let y2 : comptime uint256 = staticInt( id_uint(staticInt(42)) ); // cast at literal, cast at let

    let z = notcomptime(Typedef.rep(y1)); // no cast - not comptime
    let t = dynamic_cast(y1); // just testing
    return t;
  }

  function h() -> comptime uint256 {
    let y2 : comptime uint256 = staticInt( ( staticInt(42) ):uint256); // error w/o type annotation
    return y2;
  }
  function main() {
    return g();
  }
}
