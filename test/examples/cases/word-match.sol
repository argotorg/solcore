
trait IsWord<a> { function toWord(x : a) returns (word); }

function kw(a:word, b:word) returns (word) {return a;}

function bar<a, b>(x:(a, b)) returns (word)  where a: IsWord, b: IsWord {
  match (x ) {
  case (t,u) { return kw(IsWord.toWord(t), IsWord.toWord(u));
  } }
}
