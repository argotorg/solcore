contract SumMatchDefault {
  data Option(a) = None | Some(a);

  public function g(s : Option(word)) -> Option(word) {
    match s {
      | Option.None => return Option.None;
      | x => return x;
    }
  }

  public function main() -> word {
    g(Option.None);
    return 42;
  }
}
