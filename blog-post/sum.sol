enum uint128 { uint128(word) }

trait Sum<T> {
  function sum (x : T, y : T) returns (T);
}

impl Sum<uint128> {
  function sum(x : uint128, y : uint128) returns (uint128) {
    let res : word;
    match (x, y ) {
    case (uint128(n), uint128(m) ) {
      assembly {
        res := add(n,m)
        if lt(res, n) {
          revert(0,0)
        }
        if gt(res, 0xffffffffffffffffffffffffffffffff) {
          revert(0,0)
        }
      }
    } }
    return uint128(res);
  }
}

impl<T1, T2> Sum<(T1, T2)> where T1: Sum, T2: Sum {
  function sum (p1 : (T1, T2), p2 : (T1, T2)) returns ((T1, T2)) {
    match (p1, p2 ) {
    case ((x1,y1), (x2,y2) ) {
      return (Sum.sum(x1,x2), Sum.sum(y1,y2));
    } }
  }
}
