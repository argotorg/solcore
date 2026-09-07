// test constructor

contract Counter {

  function setCounter(v: word) public {
    assembly {
      sstore(0x00, v)
    }
  }

  function getCounter() public returns (word) {
    let res;
    assembly {
      res := sload(0x00)
    }
    return res;
  }


  constructor() {
   setCounter(42);
  }

  function main() public returns (word) {
    return getCounter();
  }
}
