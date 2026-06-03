contract Compose {
  public function compose(f,g) {
    return lam (x) {
      return f(g(x));
    } ;
  }

  public function id(x) { return x; }

  public function idid() { return compose(id,id); }

  public function main() {
    let f = compose(id,id);
    return f(42);
  }
}