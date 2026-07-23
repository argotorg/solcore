// --- preamble / duplicated std defs ---

enum Proxy<a> { Proxy }

// dynamic arrays with a runtime size. cannot exist on stack so no data constructor (i.e. should be used in combination with memory / storage pointers).
enum array<a> {}

// a typed pointer to a location in memory
enum memory<a> { memory(word) }

// word arithmetc
trait Add<t> { function add(l: t, r: t) returns (t); }
trait Mul<t> { function mul(l: t, r: t) returns (t); }
impl Add<word> {
    function add(l: word, r: word) returns (word) {
        let rw : word;
        assembly {
            rw := add(l,r)
        }
        return rw;
    }
}
impl Mul<word> {
    function mul(l: word, r: word) returns (word) {
        let rw : word;
        assembly {
            rw := mul(l,r)
        }
        return rw;
    }
}

// --- MemoryType ---

trait MemoryType<a> {
  function load(loc : word) returns (a);
  function store(loc: word, val : a) returns (());
  function size(prx : Proxy<a>) returns (word);
}

impl MemoryType<word> {
  function load(loc : word) returns (word) {
    let ret : word;
    assembly { ret := mload(loc) }
    return ret;
  }

  function store(loc : word, val : word) returns (()) {
    assembly { mstore(loc,val) }
  }

  function size(prx : Proxy<word>) returns (word) {
    return 32;
  }
}

impl<a> MemoryType<a[] memory> {
  function load(loc: word) returns (a[] memory) {
    let ret : word;
    assembly { ret := mload(loc) }
    return memory(ret);
  }

  function store(loc : word, val : a[] memory) returns (()) {
    match (val ) {
      case memory(ptr) { assembly { mstore(loc,ptr) }
    } }
  }

  function size(prx : Proxy<a memory>) returns (word) {
    return 32;
  }
}

// --- Assignment ---

trait Assign<lhs, rhs> {
  function assign(l : lhs, r : rhs) returns (());
}

impl Assign<word memory, word> {
  function assign(ptr : word memory, val : word) returns (()) {
    match (ptr ) {
      case memory(loc) { assembly {
          mstore(loc, val)
      }
    } }
  }
}

// --- Index Access ---

trait RValueIdxAccess<col_idx, val> {
  function lookup(ci : col_idx) returns (val);
}

trait LValueIdxAccess<col_idx, val> {
  function lookup(ci : col_idx) returns (val);
}

impl<a> RValueIdxAccess<(a[] memory, word), a> where a: MemoryType {
  function lookup(col_idx : (a[] memory, word)) returns (a) {
    let sz = MemoryType.size(Proxy as Proxy<a>);
    match (col_idx ) {
      case (col, idx) { match (col ) {
        case memory(loc) {
          return MemoryType.load(Add.add(loc, Mul.mul(idx, sz)));
      } }
    } }
  }
}

impl<a> LValueIdxAccess<(a[] memory, word), a memory> where a: MemoryType {
  function lookup(col_idx : (a[] memory, word)) returns (a memory) {
    let sz = MemoryType.size(Proxy as Proxy<a>);
    match (col_idx ) {
      case (col, idx) { match (col ) {
        case memory(loc) { return memory(Add.add(loc, Mul.mul(idx, sz)));
      } }
    } }
  }
}

// --- Examples ---

function main() returns (()) {
  let x : word[] memory[] memory = memory(0);
  let y : word = 0;
  let z : word[] memory = memory(0);

  let i0 : word = 0;
  let i1 : word = 1;
  let i2 : word = 2;
  let i3 : word = 3;
  let i4 : word = 4;
  let i5 : word = 5;

  // y = z[0]
  y = RValueIdxAccess.lookup((z, i0));

  //y = x[0][1]
  y = RValueIdxAccess.lookup((RValueIdxAccess.lookup((x, i0)), i1));

  //x[2][3] = x[5][4]
  Assign.assign(
    // TODO: R or L for the x[2] lookup?
    LValueIdxAccess.lookup((RValueIdxAccess.lookup((x, i2)), i3)),
    RValueIdxAccess.lookup((RValueIdxAccess.lookup((x, i5)), i4))
  );
}
