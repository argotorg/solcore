// Option type (std gap): this module should disappear once std provides it.
import * from std;

export { Option(*), unwrapOr };

enum Option<a> {
  None,
  Some(a)
}

function unwrapOr<a>(o : Option<a>, orElse : a) returns (a) {
  match (o) {
    case Option.Some(v) { return v; }
    default { return orElse; }
  }
}
