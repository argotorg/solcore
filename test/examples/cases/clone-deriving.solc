import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

forall a.
class a : Clone {
  function clone(x : a) -> a;
}

instance word : Clone {
  function clone(x : word) -> word { return x; }
}

#[derive(Clone)]
data Box = Box(word);

function cloneBox(x : Box) -> Box {
  return Clone.clone(x);
}
