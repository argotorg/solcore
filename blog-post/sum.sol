data uint128 = uint128(word);

forall T . class T : Sum {
  function sum (x : T, y : T) -> T;
}

instance uint128 : Sum {
  function sum(x : uint128, y : uint128) -> uint128 {
    let res : word;
    match x, y {
    | uint128(n), uint128(m) => 
      assembly {
        res := add(n,m);
        if lt(res, n) {
          revert(0,0);
        }
        if gt(res, 0xffffffffffffffffffffffffffffffff) {
          revert(0,0);
        }
      }
    }
    return uint128(res);
  }
}

forall T1 T2 . T1 : Sum, T2 : Sum => instance (T1,T2) : Sum {
  function sum (p1 : (T1, T2), p2 (T1, T2)) -> (T1,T2) {
    match p1, p2 {
    | (x1,y1), (x2,y2) => 
      return (Sum.sum(x1,x2), Sum.sum(y1,y2));
    }
  }
}
