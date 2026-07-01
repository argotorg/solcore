import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

data Option(a) = None | Some(a);

// Manual Generic instance without pragma no-generic-instance-for Option.
// The compiler must reject this with a conflict error.
instance Option(uint256) : Generic(sum((), uint256)) {
    function from(x : Option(uint256)) -> sum((), uint256) {
        match x {
        | Option.None    => return inl(());
        | Option.Some(v) => return inr(v);
        }
    }
    function to(r : sum((), uint256)) -> Option(uint256) {
        match r {
        | inl(_) => return Option.None;
        | inr(v) => return Option.Some(v);
        }
    }
}
