contract EitherModule {
  enum Either<a, b> { Left(a), Right(b) }
  enum List<a> { Nil, Cons(a, List<a>) }

  function lefts(xs : List<Either<word, word>>) public returns (List<word>) {
    match (xs ) {
    case List.Nil { return List.Nil ;
    } case List.Cons(y,ys) {
      match (y ) {
      case Either.Left(z) { return List.Cons(z,lefts(ys)) ;
      } case Either.Right(z) { return lefts(ys) ;
      } }
    } }
  }

  function main() public returns (word) { return 0; }
}
