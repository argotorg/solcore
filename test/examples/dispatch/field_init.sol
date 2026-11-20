// Field initializers on ADT-typed contract fields are honoured at construction
// time (Desugarer.FieldAccess.injectFieldInits). `phase` is initialised to
// Funded(7); without the initializer being honoured the field would read back
// as the zero slot, i.e. the first-declared constructor AwaitingPayment.
//
// This is the escape hatch for the SC0233 "field is never initialized" check
// (Desugarer.FieldInitialization): an ADT field must be given a value here or
// assigned in the constructor.
import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

enum Phase { AwaitingPayment, Funded(uint256), Released(uint256) }

contract Escrow {
  phase : Phase = Phase.Funded(uint256(7));

  constructor() {}

  // Discriminant of the initial state: 0 = AwaitingPayment, 1 = Funded, 2 = Released.
  function tag() public returns (uint256) {
    match (phase) {
      case Phase.AwaitingPayment { return uint256(0);
      } case Phase.Funded(_) { return uint256(1);
      } case Phase.Released(_) { return uint256(2);
      } }
  }

  // Payload carried by the initial state.
  function amount() public returns (uint256) {
    match (phase) {
      case Phase.AwaitingPayment { return uint256(0);
      } case Phase.Funded(a) { return a;
      } case Phase.Released(a) { return a;
      } }
  }
}
