import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

contract ContractLocalDerive {
  #[derive(Eq)]
  data Color = Red | Green;

  public function same() -> bool {
    return Eq.eq(Color.Red, Color.Red);
  }
}
