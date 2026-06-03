// test constructor

contract Counter {

  public function setCounter(v: word) {
    assembly {
      sstore(0x00, v)
    }
  }

  public function getCounter() -> word {
    let res;
    assembly {
      res := sload(0x00)
    }
    return res;
  }


  constructor() {
   setCounter(42);
  }

  public function main() -> word {
    return getCounter();
  }
}
