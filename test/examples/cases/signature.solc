trait Typedef<self, underlyingType> {
    function rep(x:self) returns (underlyingType);
}


function tripleFun<t>(x:t)  where t: Typedef<word> {
  return Typedef.rep(x);
}
