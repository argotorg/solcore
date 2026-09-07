function add_(x:word, y:word) { // _add is not a legal identifier :(
    let res: word;
    assembly {
       res := add(x, y)
    }
    return res;
  }

function mload_(x:word) returns (word) {
    let res: word;
    assembly {
       res := mload(x)
    }
    return res;
  }

function mstore_(a:word, v:word) {
    assembly { mstore(a,v) }
}

trait Ref<r, d> { function load(x:r) returns (d); function store(x:r, v:d) returns (());}

trait Typedef<self, underlyingType> {
    function rep(x:self) returns (underlyingType);   // abbr: x.rep = Typedef.rep(x)
    function abs(x:underlyingType) returns (self);   // abbr: x.abs
}
enum Proxy<a> { Proxy }

enum M<a> { M(word) }

impl<a> Typedef<M<a>, word> {
  function rep(m : M<a>) returns (word)  { match (m ) { case M(w) { return w; } }}
  function abs(w : word) returns (M<a>) { return M(w); }
}

trait MemoryType<Self> {
    function memorySize(p:Proxy<Self>) returns (word);
    /* inline function sizeof(Self) -> word { // an abbreviation to avoid writing Proxy; wasteful unless inlined
      return memorySize(Proxy:Proxy(self));
    } */
    function memoryStep(word, self:Self) returns (word);
    function mload(r:word) returns (Self);
    function mstore(r:word, v:Self) returns (());
}

function sizeof<Self>(self:Self) returns (word)  where Self: MemoryType {
      return MemoryType.memorySize(Proxy as Proxy<Self>);
}

trait MemoryRef<a, d> { function addr(r:a) returns (word); }
impl<a> MemoryRef<M<a>, a> { function addr(r:M<a>) returns (word) {return Typedef.rep(r);} }

function xaddr<a>(r:M<a>) returns (word) { return MemoryRef.addr(r); }
function asMemRefTo<a, b>(r:M<a>, p:Proxy<b>) returns (M<b>) { return Typedef.abs(xaddr(r)); }

function stepStore<a>(aa: word, va: a) returns (word)  where a: MemoryType {
  MemoryType.mstore(aa, va);
  return add_(aa, MemoryType.memorySize(Proxy as Proxy<a>));
}

impl<Self, r> Ref<r, Self> where Self: MemoryType, r: MemoryRef<Self> {
  function load(r:M<Self>) returns (Self) { return MemoryType.mload(xaddr(r)); }
  function store(r:M<Self>, v:Self) returns (()) { MemoryType.mstore(xaddr(r), v); }
}

impl MemoryType<word> {
  function memorySize(p:Proxy<word>) returns (word) { return 32; }
  function memoryStep(a:word, self:word) returns (word) { return add_(a,32); }
  function mload(a: word) returns (word) { return mload_(a); }
  function mstore(a: word, v:word) returns (()) { mstore_(a, v); }
}

impl<a, b> MemoryType<(a, b)> where a: MemoryType, b: MemoryType {
  function memorySize(p:Proxy<(a, b)>) returns (word) {
    return add_(MemoryType.memorySize(Proxy as Proxy<a>), MemoryType.memorySize(Proxy as Proxy<a>) );
  }

  function mload(aa:word) returns ((a, b))  {
    let va = MemoryType.mload(aa);
    let ab = add_(aa, sizeof(va));
    let vb = MemoryType.mload(ab);
    return (va,vb);
  }

  function mstore(aa:word, v: (a, b)) returns (()) {
    match (v ) { case pair(va, vb) { mstore2(aa, va, vb); } } // match-compiler cannot compile mopre than 1 stmt in a branch :(
  }
}

function mstore2<a, b>(aa:word, va:a, vb: b)  where a: MemoryType, b: MemoryType { //needed because of bug in match-compiler
  let ab = stepStore(aa, va);
  MemoryType.mstore(ab, vb);
}

enum XRef<st, field, fieldType> { XRef(st, field) }
enum PairFst { PairFst }
enum PairSnd { PairSnd }


impl<a, b, r> MemoryRef<XRef<r, PairFst, a>, a> where r: MemoryRef<(a, b)>, a: MemoryType, b: MemoryType {
  function addr(xr : XRef<r, PairFst, a>) returns (word) {
    match (xr ) { case XRef(r, _) { return MemoryRef.addr(r); } }
  }
}

impl<a, b, r> MemoryRef<XRef<r, PairSnd, b>, b> where r: MemoryRef<(a, b)>, a: MemoryType, b: MemoryType {
  function addr(xr : XRef<r, PairSnd, b>) returns (word) {
    match (xr ) {
      case XRef(r, _) { return add_(MemoryRef.addr(r), MemoryType.memorySize(Proxy as Proxy<b>));
    } }
  }
}

contract Ref219 {
  function main() public {
    let mp:M<(word, word, word)> = M(96); // no alloc yet
    let p = (1,16,25);
    Ref.store(mp, p);

    let ra = XRef(mp, PairFst);
    let a = Ref.load(ra);
    let r2 = XRef(mp, PairSnd);
    let rb = XRef(r2, PairFst);
    let a = Ref.load(ra);
    let b = Ref.load(rb);
    let rc = XRef(r2, PairSnd);
    let c = Ref.load(rc);
    return add_(a, add_(b, c));
  }
}
