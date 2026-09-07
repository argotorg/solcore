enum Pair<a, b> { Pair(a, b) }
enum Proxy<a> { Proxy }
enum Unit { Unit }

trait Typedef<a, r> {
    function abs(x:r) returns (a);
    function rep(x:a) returns (r);
}


enum uint16 { uint16(word) }

impl Typedef<uint16, word> {
  function abs(r:word) { return uint16(r);}
  function rep(x: uint16) returns (word) {
        match (x ) {
        case uint16(val) { return val;
        } }
    }
}

enum uint8 { uint8(word) }

impl Typedef<uint8, word> {
  function abs(r:word) { return uint8(r);}
  function rep(x: uint8) returns (word) {
        match (x ) {
        case uint8(val) { return val;
        } }
    }
}

enum uint256 { uint256(word) }

impl Typedef<uint256, word> {
  function abs(r:word) { return uint256(r);}
  function rep(x: uint256) returns (word) {
        match (x ) {
        case uint256(val) { return val;
        } }
    }
}


function foo(x:word) returns (uint16) {
  let result : uint16 = Typedef.abs(x);
  return result;
}


trait Convertible<self, r> {
    function convert(x:self) returns (r);
}

impl Convertible<Pair<uint8, Proxy<uint16>>, uint16> {
    function convert(p:Pair<uint8, Proxy<uint16>>) returns (uint16) {
       match (p ) {
         case Pair(x, _) { return Typedef.abs(Typedef.rep(x));
       } }
    }
}



function uint8to16(x : uint8) returns (uint16) {
  let proxy : Proxy<uint16> = Proxy;
  let result : uint16 = Convertible.convert(Pair(x,proxy));
  return result;
}

/*
forall Pair(a,Proxy(b)):Convertible(b). function  convert(x:a) -> b {
    let proxy : Proxy(b) = Proxy;
    let result : b = Convertible.convert(Pair(x,proxy));
    return result;
}
*/

function  convert<a, b>(x:a) returns (b) {
    let proxy : Proxy<b> = Proxy;
    let result : b = Convertible.convert(Pair(x,proxy));
    return result;
}

function bar(x:Unit) returns (word) {
  let result: word = convert(x);
  return result;
}


impl Convertible<Pair<uint8, Proxy<uint256>>, uint256> {
    function convert(p:Pair<uint8, Proxy<uint256>>) returns (uint256) {
       match (p ) {
         case Pair(x, _) { return Typedef.abs(Typedef.rep(x));
       } }
    }
}

impl Convertible<Pair<uint16, Proxy<uint256>>, uint256> {
    function convert(p:Pair<uint16, Proxy<uint256>>) returns (uint256) {
       match (p ) {
         case Pair(x, _) { return Typedef.abs(Typedef.rep(x));
       } }
    }
}



contract Bar {

function main() public returns (word) {
  let x = Unit;
  let y : word = convert(x);
  return y;
}

}