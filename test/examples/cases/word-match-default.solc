contract WordMatchDefault {
  public function f(n : word) -> word {
    let result : word;
    match n {
      | 0 => assembly { result := 100 }
      | x => assembly { result := x }
    }
    return result;
  }

  public function main() -> word {
    return f(42);
  }
}
