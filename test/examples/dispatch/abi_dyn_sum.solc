import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

// Minimal dynamic sum in a calldata array — like abi_batch_adt but with NO
// nested ADTs: the constructors carry primitive / bytes fields directly. This
// isolates the dynamic-sum decode path (which abi_batch_adt exercises and
// abi_array_sum does not) from nested-ADT decode (an ADT field inside an ADT,
// which abi_batch_adt also has and this test does not).
//
//   DynSum : sum(uint256, bytes)   -- dynamic (Blob carries memory(bytes))
data DynSum = Small(uint256) | Blob(memory(bytes));

contract DynSumArr {
  constructor() {}

  // The uint256 in a Small element (0 for a Blob).
  public function smallOf(items : calldata(array(DynSum)), i : uint256) -> uint256 {
    let d : DynSum = items[i];
    match d {
      | DynSum.Small(x) => return x;
      | DynSum.Blob(_)  => return uint256(0);
    }
  }

  // The bytes payload of a Blob element.
  public function blobOf(items : calldata(array(DynSum)), i : uint256) -> memory(bytes) {
    let d : DynSum = items[i];
    let out : memory(bytes);
    match d {
      | DynSum.Blob(b)  => out = b;
      | DynSum.Small(_) => revertEmpty();
    }
    return out;
  }
}
