// Regression for field initializers on ADT-typed contract fields.
//
// `On` is the SECOND constructor of `Switch`, so an untouched storage slot
// (tag 0) would read back as `Off`.  The field initializer `= Switch.On` must
// therefore be honoured at construction time (Desugarer.FieldAccess.
// injectFieldInits) for `isOn()` to return true; before that fix the
// initializer parsed but was silently discarded and `isOn()` returned false.
//
// It is also the escape hatch for the SC0233 "field is never initialized" check
// (Desugarer.FieldInitialization): `state` is a sum type, so it must be given a
// value here or assigned in the constructor.
import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

enum Switch { Off, On }

contract LightSwitch {
    state : Switch = Switch.On;

    constructor() {}

    function isOn() public returns (bool) {
        match (state) {
            case Switch.On { return true;
            } default { return false;
            } }
    }
}
