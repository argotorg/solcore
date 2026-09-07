export {Generic, CloneLike, Marker};

trait Generic<a, rep> {
  function from(value: a) returns (rep);
  function to(value: rep) returns (a);
}

trait CloneLike<a> {
  function clone(value: a) returns (a);
}

impl CloneLike<word> {
  function clone(value: word) returns (word) { return value; }
}

trait Marker<a> {}

trait Hidden<a> {}
