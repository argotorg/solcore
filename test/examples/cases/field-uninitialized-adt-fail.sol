import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

// An algebraic-data-type contract field that is neither given a field
// initializer nor assigned in the constructor is rejected (SC0233): it would
// silently zero-initialise to the first-declared constructor `AwaitingPayment`,
// a value the declaration never states.  A word-like `owner : address` field is
// fine to leave implicit and must not trigger the check.

enum Phase { AwaitingPayment, Funded(uint256), Released(uint256) }

contract Escrow {
  phase : Phase;
  owner : address;

  constructor() {}
}
