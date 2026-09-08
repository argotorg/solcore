import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;

// Complex nested-ADT ABI decode over a calldata dynamic array. The element is a
// three-level algebraic type built from sums *and* products:
//
//   Operation : sum(address, address)                -- static
//   Signature : sum((bytes32, bytes32), address)     -- static
//   Batch     : sum( (Operation, Signature)          -- Queue   : static, inline
//                  , (uint256, memory(bytes)) )       -- Execute : dynamic (carries bytes)
//
// `items[i]` dispatches through the calldata-array RValueIdxAccess instance to
// abiArrayGet, decoding the element on demand into a fully-formed `Batch` to
// match on — exercising the derived ABIDecode across three nested data types.
//
// `Batch` is a *dynamic* element (its Execute branch carries a memory(bytes)),
// so the array uses the offset-table layout: after the length word comes one
// 32-byte offset per element (relative to the element region), each pointing at
// that element's own encoding. abiArrayGet rebases onto the element start, and
// the element's inner offsets (the memory(bytes) leaf) resolve relative to the
// element — so both the static Queue path and the dynamic Execute path decode.
//
// This is currently registered via runDispatchTest, which compiles the contract
// through the whole pipeline. A runtime .json fixture (exercising the decode on
// real calldata) needs the exact solcore-generated selector for the nested-ADT
// signature, which has to be captured from a local sol-core run.

enum Operation { AddSigner(address), RemoveSigner(address) }
enum Signature { ECDSA(bytes32, bytes32), Contract(address) }
enum Batch { Queue(Operation, Signature), Execute(uint256, memory<bytes>) }

// Address added by an AddSigner op (address(0) for a RemoveSigner).
function addedSigner(op : Operation) returns (address) {
    match (op ) {
      case Operation.AddSigner(a)    { return a;
      } case Operation.RemoveSigner(_) { return address(0);
    } }
}

// Verifying contract address of a Contract signature (address(0) for ECDSA).
function contractVerifier(sig : Signature) returns (address) {
    match (sig ) {
      case Signature.Contract(a) { return a;
      } case Signature.ECDSA(_, _) { return address(0);
    } }
}

contract BatchDecoder {
  constructor() {}

  // From a Queue(AddSigner(a), Contract(c)) element, return (a, c): the signer
  // being added and the contract that verifies the queued action.
  function queueSigner(items : calldata<array<Batch>>, i : uint256) public returns ((address, address)) {
    let b : Batch = items[i];
    match (b ) {
      case Batch.Queue(op, sig) { return (addedSigner(op), contractVerifier(sig));
      } case Batch.Execute(_, _)  { return (address(0), address(0));
    } }
  }

  // The payload bytes carried by an Execute element.
  function execPayload(items : calldata<array<Batch>>, i : uint256) public returns (memory<bytes>) {
    let b : Batch = items[i];
    let out : memory<bytes>;
    match (b ) {
      case Batch.Execute(_, payload) { out = payload;
      } case Batch.Queue(_, _)         { revertEmpty();
    } }
    return out;
  }
}
