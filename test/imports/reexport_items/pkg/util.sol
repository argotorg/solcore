export {Wrap(*), unwrap, Unbox};

enum Wrap { Mk(word) }

trait Unbox<self> {
  function unbox(x:self) returns (word);
}

impl Unbox<Wrap> {
  function unbox(x:Wrap) returns (word) {
    match (x ) {
    case Wrap.Mk(w) { return w;
    } }
  }
}

function unwrap(x:Wrap) returns (word) {
  return Unbox.unbox(x);
}
