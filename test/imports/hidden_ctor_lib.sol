export {Token(Ok), mkOk, mkErr};

enum Token { Ok(word), Err(word) }

function mkOk(x: word) returns (Token) {
  return Token.Ok(x);
}

function mkErr(x: word) returns (Token) {
  return Token.Err(x);
}
