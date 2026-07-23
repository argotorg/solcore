pragma solcore noCoverageCondition Nth;


enum Zero {}
enum Succ<a> {}

enum Proxy<a> { Proxy }

trait Nth<a, b, c> {
  function nth (x : Proxy<a>, y : b) returns (c);
}

impl<a, b> Nth<Zero, (a, b), a> {
   function nth (x : Proxy<Zero>, y : (a, b)) returns (a) {
      match (y ) {
      case (a, b) { return a ;
      } }
   }
}

impl<n, a, b, c> Nth<Succ<n>, (a, b), c> where n: Nth<b, c> {
   function nth (x : Proxy<Succ<n>>, y : (a, b)) returns (c) {
      match (y ) {
      case (a,b) { return Nth.nth(Proxy as Proxy<n>, b);
      } }
   }
}

contract C {
  function id (x : word) public returns (word) {
    return x;
  }
  function main () public returns (()) {
    let p : (word, word, word, ());
    let x : word = Nth.nth(Proxy as Proxy<Zero>, p);
    let y : word = Nth.nth(Proxy as Proxy<Succ<Zero>>, p);
    let z : word = Nth.nth(Proxy as Proxy<Succ<Succ<Zero>>>, p);
    id(z);
  }
}

