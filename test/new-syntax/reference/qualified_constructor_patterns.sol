contract QualifiedConstructorPatterns {
  enum Option<a> { None, Some(a) }

  function join<a>(mmx: Option<Option<a>>) returns (Option<a>) {
    match (mmx) {
case Option.None {
return Option.None;
}
case Option.Some(Option.Some(x)) {
return Option.Some(x);
}
case Option.Some(Option.None) {
return Option.None;
}
}
  }
}
