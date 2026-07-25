trait Ref<ref, deref> {
  function load (r:ref) returns (deref);
  function store(r:ref, v:deref) returns (unit);
}

enum stack<a> { stack(a) }

impl Ref<stack<a>, a> {
}

enum MemberAccess<ty, field> { MemberAccess(ty) }

enum PairFst { PairFst }
enum PairSnd { PairSnd }

enum XRef<st, field, fieldType> { XRef(st, field) }
impl<r, a, b> Ref<XRef<r, PairFst, a>, a> where r: Ref<a, b> {}
impl<r, a, b> Ref<XRef<r, PairSnd, b>, b> where r: Ref<a, b> {}

contract AssignNested {
  function main() public {
    let x : stack<(word, (word, word))>;
    let z : stack<(word, (word, word))>;

    // either of the next lines is fine on their own, but not together
    Ref.store( XRef(z,PairFst), 21);
    Ref.store( XRef(XRef(x, PairSnd), PairFst), 20 );

    return 77;
  }
}
