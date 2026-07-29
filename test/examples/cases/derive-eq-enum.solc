import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq, Ord)]
data Color = Red | Green | Blue;

function sameColor() -> bool {
    return Eq.eq(Color.Red, Color.Red);
}

function diffColor() -> bool {
    return ne(Color.Red, Color.Blue);
}

function ordering() -> bool {
    match Ord.gt(Color.Green, Color.Red) {
    | true  => return not(Ord.gt(Color.Red, Color.Green));
    | false => return false;
    }
}
