import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

// Companion to field-uninitialized-adt-fail.sol: the ways an ADT field may
// satisfy the initialisation check (SC0233), plus the exemptions.

enum Phase { AwaitingPayment, Funded(uint256), Released(uint256) }

// A newtype over a scalar is primitive-like (unambiguous zero), so a field of
// it may be left uninitialised.
enum Id { Id(uint256) }

contract Escrow {
  // ok: assigned in the constructor.
  phase : Phase;
  // ok: primitive-like newtype, implicit zero allowed.
  tag : Id;
  // ok: word-like scalar, implicit zero allowed.
  owner : address;
  // ok: phantom/dynamic type, defaults to empty.
  note : bytes;

  constructor() {
    phase = Phase.AwaitingPayment;
  }
}
