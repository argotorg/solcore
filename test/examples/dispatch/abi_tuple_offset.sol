import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract AbiTupleOffset {
  constructor() {}

  function encodeTupleAfterField() public returns (memory<bytes>) {
    let v : (uint256, ABITuple<memory<DynArray<uint256>>>) = (9, ABITuple([5, 6]));
    return abi_encode(v);
  }

  function encodeTupleInSum() public returns (memory<bytes>) {
    let v : sum<ABITuple<ABITuple<memory<DynArray<uint256>>>>, ()> = inl(ABITuple(ABITuple([])));
    return abi_encode(v);
  }
}
