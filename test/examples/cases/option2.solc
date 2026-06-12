contract Option {
  data Option(a) = None | Some(a);

  public function just(x : word) -> Option(word) { return Option.Some(x); }

  public function maybe(n : word, o : Option(word)) -> word {
    match o {
      | Option.None => return n;
      | Option.Some(x) => return x;
    }
  }

  public function join(mmx : Option(Option(word))) -> Option(word) {
    match mmx {
      | Option.None => return Option.None;
      | Option.Some(Option.None) => return Option.None;
      | Option.Some(Option.Some(x)) => return Option.Some(x);
    }
  }

  public function join2(mmx : Option(Option(word))) -> Option(word) {
    match mmx {
      | Option.Some(m) => match m {
          | Option.None => return Option.None;
          | Option.Some(x) => return Option.Some(x);
      }
      | _ => return Option.None;
    }
  }

  public function main() -> word {
   //  return maybe(0, join(Option.Some(Option.Some(42))));
   return 42;
  }
}
