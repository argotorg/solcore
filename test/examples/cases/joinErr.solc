contract Option {
  data Option(a) = None | Some(a);
  data Bool = False | True;

  public function maybe(n : word, o : Option(word)) -> word {
    match o {
      | Option.None => return n;
      | Option.Some(x) => return x;
    }
  }

  public function join(mmx : Option(Option(word))) -> Option(word) {
    let result = Option.None;
    match mmx {
      | Option.Some(Option.Some(x)) => result = Option.Some(x);
      | Option.None => result = Option.None;
    }
    return result;
  }


  public function main() -> word {
    return maybe(0, join(Option.Some(Option.Some(Bool.False))));
  }
}
