function inc(x : word) returns (word) {
  let f = lambda () {
    let res : word ; 
    assembly {
      res := add(x,1)
    }
    return res;
  } ; 
  return f();
}

contract Foo {

  function main () public returns (word) {
    return inc(0);
  }  
}
