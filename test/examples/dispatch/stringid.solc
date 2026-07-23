import std.{*};
import std.dispatch.{*};
import std.opcodes.{mstore, mload};

contract C {
    constructor() {}
    public function id(x:memory(string)) -> (memory(string)) {
     let ptr : word = Typedef.rep(x);
     let len : word;
     let n1 : word;
     assembly {
       len := mload(ptr)
       n1 := mload(add(ptr,32))
     }
     log1(len, 0xc001);
     log1(n1, 0xc002);

     return x;
    }

    public function const_a() -> (memory(string)) {
      let resPtr = allocate_memory(64);
      let payload : word = 0x7777777777777777777777777777777777777777777777777777777777777777;
      mstore(resPtr, 3);
      mstore(resPtr+32, payload);
      return memory(resPtr);
    }
    public function mylen(x:memory(string)) -> uint256 {
     let ptr : word = Typedef.rep(x);
     let l : word;
     let n1 : word;
     assembly {
       l := mload(ptr)
       n1 := mload(add(ptr,32))
     }
     // log1(l, 0xc001);
     // log1(n1, 0xc002);

     return uint256(l);
    }

  //  function answer() -> uint256 { return uint256(17); }
}
