// Terminators (stop, invalid, selfdestruct, revert) never return control, so
// a bare 'assembly' block ending in one is allowed as the last statement of a
// value-returning function — its polymorphic result unifies with any return
// type. Regression test for stop/invalid/selfdestruct being made polymorphic
// like revert/return (see Primitives.hs 'yulPrimOps').

function viaStop<a>() returns (a) {
  assembly {
    stop()
  }
}

function viaInvalid<a>() returns (a) {
  assembly {
    invalid()
  }
}

function viaSelfdestruct<a>(beneficiary: word) returns (a) {
  assembly {
    selfdestruct(beneficiary)
  }
}

function viaRevert<a>() returns (a) {
  assembly {
    revert(0, 0)
  }
}

function useWord(w: word) returns (()) {}

contract Terminators {
  function main() public returns (()) {
    useWord(viaStop());
    useWord(viaInvalid());
    useWord(viaSelfdestruct(0));
    useWord(viaRevert());
  }
}
