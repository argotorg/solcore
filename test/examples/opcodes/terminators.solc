// Terminators (stop, invalid, selfdestruct, revert) never return control, so
// a bare 'assembly' block ending in one is allowed as the last statement of a
// value-returning function — its polymorphic result unifies with any return
// type. Regression test for stop/invalid/selfdestruct being made polymorphic
// like revert/return (see Primitives.hs 'yulPrimOps').

forall a.function viaStop() -> a {
  assembly {
    stop()
  }
}

forall a.function viaInvalid() -> a {
  assembly {
    invalid()
  }
}

forall a.function viaSelfdestruct(beneficiary: word) -> a {
  assembly {
    selfdestruct(beneficiary)
  }
}

forall a.function viaRevert() -> a {
  assembly {
    revert(0, 0)
  }
}

function useWord(w: word) -> () {}

contract Terminators {
  public function main() -> () {
    useWord(viaStop());
    useWord(viaInvalid());
    useWord(viaSelfdestruct(0));
    useWord(viaRevert());
  }
}
