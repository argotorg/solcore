trait Typedef<self, underlyingType> {
    function rep(x:self) returns (underlyingType);
    function abs(x:underlyingType) returns (self);
}

function tripleFun<t>(x:t) returns ((word, (word, word)))  where t: Typedef<(word, (word, word))> {
    return Typedef.rep(x);
  }
