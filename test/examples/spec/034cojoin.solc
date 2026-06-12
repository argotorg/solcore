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
    let result = Option.None;
    match mmx {
      | Option.Some(Option.Some(x)) => result = Option.Some(x);
      | Option.None => result = Option.None;
      | Option.Some(Option.None) => result = Option.None;
      | _ => result = Option.None;
    }
    return result;
  }

 public function extract(mx : Option(word)) -> word {
   match mx {
     | Option.Some(x) => return x;
     | Option.None => return 0;
   }
 }

  public function cojoin(x : Option(word)) -> Option(Option(word)) { // Test that sum types can grow
    let result = Option.None;
    result = Option.Some(x);
    return result;
   }


  public function main() -> word {
    return maybe(0, join(cojoin(Option.Some(42))));
  }
}
