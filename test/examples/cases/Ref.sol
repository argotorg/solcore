trait Ref<ref, deref> {
  function load (r : ref) returns (deref);
  function store (r : ref, d : deref) returns (unit);
}

enum Memory<a> { new(a) }

impl Ref<Memory<a>, a> {
  function load (r) {
    match (r ) {
    case Memory.new(x) { return x;
    } }
  }
}


