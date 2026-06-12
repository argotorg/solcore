contract Option {
  data Option(a) = None | Some(a);

  public function maybe(n : word, o : Option(word)) -> word {
    match o {
      | Option.Some(x) => return x;
      | Option.None => return n;
    }
  }

  public function main() -> word {
    return maybe(7, Option.None);
  }
}
