import {add, sub, mul, div, mod, addmod as addmod_, mulmod as mulmod_, and as and_, or as or_, xor as xor_, shl, shr, eq, not as not_, gt as gt_, iszero, keccak256, mstore, mload, mcopy, sstore, sload, gas, calldataload, calldatacopy, returndatasize, returndatacopy, log1 as log1_, call, staticcall, revert_, invalid} from std.opcodes;

pragma no-patterson-condition ABIEncode, Num, Array, ArrayPush, Eq, Ord;
pragma no-coverage-condition ABIDecode, MemoryType, Array, ArrayPush, RValueIdxAccess;

export {
  ABIAttribs,
  ABIDecode,
  ABIDecoder(*),
  ABIEncode,
  ABITuple(*),
  Add,
  Array,
  ArrayPush,
  Assign,
  BitAnd,
  BitNot,
  BitOr,
  BitXor,
  Bounded,
  CalldataWordReader(*),
  CanStore,
  ContractStorage(*),
  Div,
  DynArray,
  Error(*),
  Eq,
  HasWordReader,
  IndexAccess,
  LVA,
  LValueIdxAccess,
  Length,
  MemberAccessProxy(*),
  MemoryEncode,
  MemoryPointer,
  MemorySize,
  MemoryType,
  MemoryWordReader(*),
  Mod,
  Mul,
  Num,
  Ord,
  Proxy(*),
  RVA,
  RValueIdxAccess,
  StorageCopy,
  StorageSize,
  StorageType,
  StructField(*),
  Sub,
  Typedef,
  WordReader,
  abi_decode,
  abi_encode,
  absurd,
  addWord,
  addmod,
  allocateDynamicArray,
  address(*),
  allocate_memory,
  allocate_zeroed_memory,
  and,
  array(*),
  arrayLitInit,
  arrayLitNew,
  assert,
  byte(*),
  bytes,
  bytes4(*),
  bytes32(*),
  bandWord,
  borWord,
  bxorWord,
  bnotWord,
  bshlWord,
  bshrWord,
  calldata(*),
  concat,
  concatLit,
  ecrecover,
  empty(*),
  eqWord,
  erc7201,
  frombool,
  ge,
  getReader,
  get_free_memory,
  gt,
  gtWord,
  hash1,
  hash2,
  keccak256_,
  keccakLit,
  keccakWordLit,
  le,
  lidx,
  loadBytesFromStorage,
  log1,
  lt,
  mapping(*),
  maxVal,
  maxWord,
  memberAccessBase,
  memory(*),
  memory_ref,
  minWord,
  mulmod,
  ne,
  not,
  or,
  out_of_bounds,
  raw_call,
  readStorage,
  returndata(*),
  revertLit,
  revertEmpty,
  revertWithError,
  require,
  ridx,
  ripemd160,
  round_up_to_mul_of_32,
  rval,
  set_free_memory,
  sha256,
  slice(*),
  slice_,
  storage(*),
  storeArrayLit,
  storeBytesFromMemory,
  string,
  strlen,
  strlenLit,
  subWord,
  truncate,
  toWord,
  to_bytes,
  tobool,
  uint256(*),
  unimplemented,
  zeroize_memory
};

/*
- features
    - primitive word eq
    - include stdlib
    - MPTC + optional weak args (MPTC formalization?)
    - surface for loops
    - better inference for Typedef.rep() calls (have to annotate atm?)
    - boolean short circuiting
- sugar
    - Proxy (e.g. `@t ==> Proxy : Proxy t`
    - IndexAccess reads (e.g. `x[i] ==> IndexAccess.get(x, i)`)
    - auto typedef instances
- syntax
    - order of type args
    - braces for blocks in matches
    - trait / impl vs class / instance
    - function -> fn?
    - assembly vs high level return?
- todo
    - abi decoding
    - contract desugaring
    - mappings
    - strings
    - full range of uintX / intX / bytesX types
    - address types
    - statically sized arrays
    - tuple field access
    - structs
    - define numeric tower
    - fixed point types
    - fixed point numeric routines
    - memory vectors
*/


function log1<t>(v:t, topic:word) returns (())  where t: Typedef<word> {
  let w : word = Typedef.rep(v);
  mstore(0, w);
  log1_(0, 32, topic);
}

function unimplemented() returns (()) {
  let Unimplemented = Error(0x6e128399);
  revertWithError(Unimplemented);
}

function out_of_bounds() returns (()) {
    let OutOfBounds = Error(0xb4120f14);
    revertWithError(OutOfBounds);
}

// ------------------------------------------------------------------
// High-level revert helper
// ------------------------------------------------------------------
// EmitHull has special handling for `revertLit("...")` after MastEval has
// constant-folded the argument to a string literal.
function revertLit(comptime s: string) returns (()) {
    unimplemented(); // Sanity check if folding ignores it.
    return;
}

// Empty revert.
function revertEmpty() returns (()) {
    revert_(0, 0);
}

// Bottom: a value of any type. absurd never returns, it reverts, so it can
// stand in for a result of any type. Used to derive class instances for empty
// data types (which have no values, so the method bodies are unreachable). The
// recursive tail satisfies the forall a . a return type; execution never
// reaches it because revertEmpty() aborts first.
function absurd<a>() returns (a) {
    // Despite looking like an infinite loop, this reverts: revertEmpty()
    // aborts execution on the first line, so the recursive return absurd()
    // is never actually run. The recursion exists only to give the body a
    // value of type a, satisfying the forall a . a return type.
    revertEmpty();
    return absurd();
}

// TODO: use bytes4
enum Error { Error(word), Empty, Msg(memory<string>) }

// A string literal can be used as an Error: `require(cond, "message")` reverts
// with the message.  The literal is materialized into memory(string) here; MastEval
// erases the comptime-only parameter by cloning this method per literal, so
// the materializer sees a literal rather than a parameter.
impl Str<Error> {
    function fromString(s: string) returns (Error) {
        return Error.Msg(Str.fromString(s));
    }
}

// Revert with Error selector.
function revertWithError(e:Error) returns (()) {
    match (e ) {
        case .Error(selector) {
            mstore(0, selector);
            // We only care about the BE MSB.
            revert_(28, 4);
        } case .Empty {
            revert_(0, 0);
        } case .Msg(msg) {
            let msg_ = Typedef.rep(msg);
            revert_(msg_ + 32, mload(msg_));
    } }
}

function assert(cond: bool) returns (()) {
    if (!cond) {
        invalid();
    }
}

function require(cond: bool, e: Error) returns (()) {
    if (!cond) {
        revertWithError(e);
    }
}

// --- booleans ---

// TODO: this should short circuit. probably needs some compiler magic to do so.
function and(x: bool, y: bool) returns (bool) {
    match (x, y ) {
    case (true, y ) { return y;
    } case (false, _ ) { return false;
    } }
}

// TODO: this should short circuit. probably needs some compiler magic to do so.
function or(x: bool, y: bool) returns (bool) {
    match (x, y ) {
    case (true, _ ) { return true;
    } case (false, y ) { return y;
    } }
}

function not(b:bool) returns (bool) {
  match (b ) {
    case false { return true;
    } case true { return false;
  } }
}

function frombool(b : bool) returns (word) {
 match (b ) {
   case false { return 0;
   } case true { return 1;
 } }
}

function tobool(x: word) returns (bool) {
  match (x ) {
    case 0 { return false;
    } default { return true;
  } }
}

// --- Tuple projections ---

function fst<a, b>(p: (a, b)) returns (a) {
    match (p ) {
    case (a, _) { return a;
    } }
}

function snd<a, b>(p: (a, b)) returns (b) {
    match (p ) {
    case (_, b) { return b;
    } }
}

// --- Proxy ---

// Proxy is a unit type that can be used to pass Types as paramaters at runtime
enum Proxy<t> { Proxy }

// --- Type Abstraction ---

trait Typedef<abs, rep> {
    function abs(x:rep) returns (abs);
    function rep(x:abs) returns (rep);
}

default impl<t> Typedef<t, t> {
    function abs(x:t) returns (t) { return x; }
    function rep(x:t) returns (t) { return x; }
}

// --- Equality ---
// Note: All these are used by the compiler by name.

trait Eq<a> {
  function eq(x:a, y:a) returns (bool);
}

function ne<a>(x:a, y:a) returns (bool)  where a: Eq {
  return not(Eq.eq(x,y));
}

// --- Ordering ---
// Note: All these are used by the compiler by name.

trait Ord<a> where a: Eq {
  function gt(x:a, y:a) returns (bool);
}

function gt<a>(x:a, y:a) returns (bool)  where a: Ord {
  return Ord.gt(x,y);
}

function le<a>(x:a, y:a) returns (bool)  where a: Ord {
  return not(Ord.gt(x,y));
}

function ge<a>(x:a, y:a) returns (bool)  where a: Ord {
  return le(y,x);
}

function lt<a>(x:a, y:a) returns (bool)  where a: Ord {
    return Ord.gt(y,x);
}

// --- Generic deriving: structural instances over the representation universe ---
// These let `#[derive(Eq)]` / `#[derive(Ord)]` work for any data type through
// its Generic(rep) instance, where rep is built from (), sum(f, g) and (f, g).

impl Eq<()> {
  function eq(x : (), y : ()) returns (bool) {
    return true;
  }
}

impl<f, g> Eq<sum<f, g>> where f: Eq, g: Eq {
  function eq(x : sum<f, g>, y : sum<f, g>) returns (bool) {
    match (x ) {
    case inl(a) {
        match (y ) {
        case inl(b) { return Eq.eq(a, b);
        } case inr(b) { return false;
        } }
    } case inr(a) {
        match (y ) {
        case inl(b) { return false;
        } case inr(b) { return Eq.eq(a, b);
        } }
    } }
  }
}

impl<f, g> Eq<(f, g)> where f: Eq, g: Eq {
  function eq(x : (f, g), y : (f, g)) returns (bool) {
    match (x ) {
    case (a1, b1) {
        match (y ) {
        case (a2, b2) {
            match (Eq.eq(a1, a2) ) {
            case true  { return Eq.eq(b1, b2);
            } case false { return false;
            } }
        } }
    } }
  }
}

impl Ord<()> {
  function gt(x : (), y : ()) returns (bool) {
    return false;
  }
}

impl<f, g> Ord<sum<f, g>> where f: Ord, g: Ord {
  function gt(x : sum<f, g>, y : sum<f, g>) returns (bool) {
    match (x ) {
    case inl(a) {
        match (y ) {
        case inl(b) { return Ord.gt(a, b);
        } case inr(b) { return false;
        } }
    } case inr(a) {
        match (y ) {
        case inl(b) { return true;
        } case inr(b) { return Ord.gt(a, b);
        } }
    } }
  }
}

impl<f, g> Ord<(f, g)> where f: Ord, g: Ord {
  function gt(x : (f, g), y : (f, g)) returns (bool) {
    match (x ) {
    case (a1, b1) {
        match (y ) {
        case (a2, b2) {
            match (Ord.gt(a1, a2) ) {
            case true  { return true;
            } case false {
                match (Eq.eq(a1, a2) ) {
                case true  { return Ord.gt(b1, b2);
                } case false { return false;
                } }
            } }
        } }
    } }
  }
}

// --- Arithmetic ---
// Note: All these are used by the compiler by name.

trait Add<t> {
    function add(l: t, r: t) returns (t);
}

trait Sub<t> {
    function sub(l: t, r: t) returns (t);
}

trait Mul<t> {
    function mul(l: t, r: t) returns (t);
}

trait Div<t> {
    function div(l: t, r: t) returns (t);
}

trait Mod<t> {
    function mod(l: t, r: t) returns (t);
}

trait BitAnd<t> {
    function band(l: t, r: t) returns (t);
}

trait BitOr<t> {
    function bor(l: t, r: t) returns (t);
}

trait BitXor<t> {
    function bxor(l: t, r: t) returns (t);
}

trait BitNot<t> {
    function bnot(x: t) returns (t);
}

trait Bounded<t> {
  function minVal() returns (t);
  function maxVal() returns (t);
}

function maxVal<t>() returns (t)  where t: Bounded { return Bounded.maxVal(); }

// umbrella class
trait Num<a> where a: Add, a: Sub, a: Bounded, a: Eq, a: Ord, a: Typedef<word> {
  function maxVal() returns (a);
  function toWord(x:a) returns (word);
  function fromWord(x:word) returns (a);
  function fromInteger(comptime x:integer) returns (comptime<a>);
  function add(x:a, y:a) returns (a);
  function sub(x:a, y:a) returns (a);
  function gt(x:a, y:a) returns (bool);
}

default impl<a> Num<a> where a: Add, a: Sub, a: Bounded, a: Eq, a: Ord, a: Typedef<word> {
  function maxVal() returns (a) { return Bounded.maxVal(); }
  function toWord(x:a) returns (word) { return Typedef.rep(x); }
  function fromWord(x:word) returns (a) { return Typedef.abs(x); }
  function fromInteger(comptime x:integer) returns (comptime<a>) { return Typedef.abs(wordFromInteger(x)); }
  function add(x:a, y:a) returns (a) { return Add.add(x,y); }
  function sub(x:a, y:a) returns (a) { return Sub.sub(x,y); }
  function gt(x: a, y: a) returns (bool) { return Ord.gt(x, y); }
}

// --- Word Arithmetic & Logic ---
// TODO: make these checked

// These are intended to be folded by MastEval when their arguments are
// statically known word values.
function eqWord(x:word, y:word) returns (bool) {
    return tobool(eq(x, y));
}

function gtWord(x:word, y:word) returns (bool) {
    return tobool(gt_(x, y));
}

function maxWord(a : word, b : word) returns (word) {
    match (gtWord(a, b) ) {
    case true  { return a;
    } case false { return b;
    } }
}

function minWord(a : word, b : word) returns (word) {
    match (gtWord(a, b) ) {
    case true  { return b;
    } case false { return a;
    } }
}

function addWord(l: word, r: word) returns (word) {
    return add(l, r);
}

function subWord(l: word, r: word) returns (word) {
    return sub(l, r);
}

// Bitwise AND
function bandWord(x: word, y: word) returns (word) {
    return and_(x, y);
}

// Bitwise OR
function borWord(x: word, y: word) returns (word) {
    return or_(x, y);
}

// Bitwise XOR
function bxorWord(x: word, y: word) returns (word) {
    return xor_(x, y);
}

// Bitwise NOT
function bnotWord(x: word) returns (word) {
    return not_(x);
}

// Bitwise SHL
function bshlWord(x: word, y: word) returns (word) {
    return shl(x, y);
}

// Bitwise SHR
function bshrWord(x: word, y: word) returns (word) {
    return shr(x, y);
}

impl Eq<word> {
  function eq(x:word, y:word) returns (bool) {
    return eqWord(x, y);
  }
}

impl Ord<word> {
  function gt(x:word, y:word) returns (bool) {
    return gtWord(x, y);
  }
}

impl Add<word> {
    function add(l: word, r: word) returns (word) {
        return addWord(l, r);
    }
}

impl Sub<word> {
    function sub(l: word, r: word) returns (word) {
        return subWord(l, r);
    }
}

function mulWord(l: word, r: word) returns (word) {
    return mul(l, r);
}

impl Mul<word> {
    function mul(l: word, r: word) returns (word) {
        return mulWord(l, r);
    }
}

impl Div<word> {
    function div(l: word, r: word) returns (word) {
        return div(l, r);
    }
}

impl Mod<word> {
  function mod (l : word, r : word) returns (word) {
    return mod(l, r);
  }
}

impl BitAnd<word> {
    function band(l: word, r: word) returns (word) {
        return bandWord(l, r);
    }
}

impl BitOr<word> {
    function bor(l: word, r: word) returns (word) {
        return borWord(l, r);
    }
}

impl BitXor<word> {
    function bxor(l: word, r: word) returns (word) {
        return bxorWord(l, r);
    }
}

impl BitNot<word> {
    function bnot(x: word) returns (word) {
        return bnotWord(x);
    }
}

impl Eq<integer> {
  function eq(x : integer, y : integer) returns (bool) {
    return integerEq(x, y);
  }
}

impl Ord<integer> {
  function gt(x : integer, y : integer) returns (bool) {
    return integerLt(y, x);
  }
}

impl Add<integer> {
  function add(l : integer, r : integer) returns (integer) {
    return integerAdd(l, r);
  }
}

impl Sub<integer> {
  function sub(l : integer, r : integer) returns (integer) {
    return integerSub(l, r);
  }
}

impl Mul<integer> {
  function mul(l : integer, r : integer) returns (integer) {
    return integerMul(l, r);
  }
}

impl Bounded<word> {
  function maxVal() returns (word) {
    return 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
  }
  function minVal () returns (word) {
    return 0;
  }
}

function hash1(x: word) returns (word) {
    mstore(0, x);
    return keccak256(0, 32);
}

function hash2(x: word, y: word) returns (word) {
    mstore(0, x);
    mstore(32, y);
    return keccak256(0, 64);
}

// --- Value Types ---

function toWord<t>(x:t) returns (word)  where t: Typedef<word> { return Typedef.rep(x); }

enum uint256 { uint256(word) }
impl Typedef<uint256, word> {
    function abs(w: word) returns (uint256) {
        return uint256(w);
    }

    function rep(x: uint256) returns (word) {
        match (x ) {
        case uint256(w) { return w;
        } }
    }
}
impl Add<uint256> {
  function add(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(Add.add(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Sub<uint256> {
  function sub(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(Sub.sub(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Mul<uint256> {
  function mul(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(Mul.mul(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Div<uint256> {
  function div(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(Div.div(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Mod<uint256> {
  function mod(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(Mod.mod(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitAnd<uint256> {
  function band(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(BitAnd.band(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitOr<uint256> {
  function bor(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(BitOr.bor(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitXor<uint256> {
  function bxor(x : uint256, y : uint256) returns (uint256) {
    return Typedef.abs(BitXor.bxor(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitNot<uint256> {
  function bnot(x : uint256) returns (uint256) {
    return Typedef.abs(BitNot.bnot(Typedef.rep(x)));
  }
}

impl Eq<uint256> {
  function eq(x : uint256, y : uint256) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<uint256> {
  function gt(x : uint256, y : uint256) returns (bool) {
    return Ord.gt(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Bounded<uint256> {
  function maxVal() returns (uint256) {
    return uint256(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);
  }
  function minVal () returns (uint256) {
    return uint256(0);
  }
}

impl Int<uint256> {
  function fromInteger(x:integer) returns (uint256) {
    return uint256(wordFromInteger(x));
  }
}

function addmod(x: uint256, y: uint256, k: uint256) returns (uint256) {
    require(k != uint256(0), Error(0x7125cbb9)); // AddModWithZero()
    return Typedef.abs(addmod_(Typedef.rep(x), Typedef.rep(y), Typedef.rep(k)));
}

function mulmod(x: uint256, y: uint256, k: uint256) returns (uint256) {
    require(k != uint256(0), Error(0xdaea23b9)); // MulModWithZero()
    return Typedef.abs(mulmod_(Typedef.rep(x), Typedef.rep(y), Typedef.rep(k)));
}

enum byte { byte(word) }
impl Typedef<byte, word> {
    function abs(w: word) returns (byte) {
        return byte(w);
    }

    function rep(x: byte) returns (word) {
        match (x ) {
        case byte(w) { return w;
        } }
    }
}

// --- Address ---
enum address { address(word) }

impl Typedef<address, word> {
    function rep(x:address) returns (word) {
        match (x ) {
            case address(y) { return y;
        } }
    }
    function abs(x:word) returns (address) {
        return address(x);
    }
}

impl Eq<address> {
  function eq(x : address , y : address) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

// --- Bytes4 ---

enum bytes4 { bytes4(word) }

impl Typedef<bytes4, word> {
    function rep(b : bytes4) returns (word) {
        match (b ) {
            case bytes4(w) { return w;
        } }
    }
    function abs(w : word) returns (bytes4) {
        return bytes4(w);
    }
}

// --- Bytes32 ---

enum bytes32 { bytes32(word) }

impl Typedef<bytes32, word> {
    function rep(b : bytes32) returns (word) {
        match (b ) {
            case bytes32(w) { return w;
        } }
    }
    function abs(w : word) returns (bytes32) {
        return bytes32(w);
    }
}

impl Eq<bytes32> {
  function eq(x : bytes32, y : bytes32) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<bytes32> {
  function gt(x : bytes32, y : bytes32) returns (bool) {
    return Ord.gt(Typedef.rep(x), Typedef.rep(y));
  }
}

// --- Pointers ---

enum memory<t> { memory(word) }
impl<t> Typedef<memory<t>, word> {
    function abs(x: word) returns (memory<t>) {
        return memory(x);
    }

    function rep(x: memory<t>) returns (word) {
        match (x ) {
        case memory(w) { return w;
        } }
    }
}

enum storage<t> { storage(word) }
impl<t> Typedef<storage<t>, word> {
    function abs(x: word) returns (storage<t>) {
        return storage(x);
    }

    function rep(x: storage<t>) returns (word) {
        match (x ) {
        case storage(w) { return w;
       } }
    }
}

enum calldata<t> { calldata(word) }
impl<t> Typedef<calldata<t>, word> {
    function abs(x: word) returns (calldata<t>) {
        return calldata(x);
    }

    function rep(x: calldata<t>) returns (word) {
        match (x ) {
        case calldata(w) { return w;
       } }
    }
}

enum returndata<t> { returndata(word) }
impl<t> Typedef<returndata<t>, word> {
    function abs(x: word) returns (returndata<t>) {
        return returndata(x);
    }

    function rep(x: returndata<t>) returns (word) {
        match (x ) {
        case returndata(w) { return w;
       } }
    }
}

enum mapping<member, index> { mapping(word) }

enum array<member> { array(word) }

// --- Low-level memory ops

function strlen(s:memory<string>) returns (word) {
  match (s ) { case memory(a) { return mload(a); } }
}

// --- Memory Utilities ---

// Memory in solidity is bump allocated in a single arena
// The word stored in memory at index 0x40 is used to store the start of the currently unused memory region

// returns the value stored in memory(0x40)
function get_free_memory() returns (word) {
    return mload(0x40);
}

// set the value stored in memory(0x40)
function set_free_memory(loc : word) returns (()) {
    mstore(0x40, loc);
}

// Allocate memory and update the memory pointer.
function allocate_memory(size : word) returns (word) {
    let ptr = get_free_memory();
    set_free_memory(ptr + size);
    return ptr;
}

function allocate_zeroed_memory(size: word) returns (word) {
    let ptr = allocate_memory(size);
    zeroize_memory(ptr, size);
    return ptr;
}

// Clears a memory area.
function zeroize_memory(ptr: word, len: word) returns (()) {
    let end_ptr = ptr + len;

    // Zero out 32-byte words.
    for (let i = 0; i < len / 32; i += 1, ptr += 32) {
        mstore(ptr, 0);
    }

    // Zero out trailing bytes. We rely on the zero-slot (0x60-0x7f).
    mcopy(ptr, 0x60, end_ptr - ptr);
}

// --- Indexable Types ---

// types that can be written to and read from at a uint256 index
// TODO: this needs to be split into LValue / RValue variants for `=` desugaring
trait IndexAccess<t, val> {
    function get(c: t, i: uint256) returns (val);
    function set(c: t, i: uint256, v: val) returns (());
}

// --- DynArray ---

// Word arrays with a size known only at runtime
// types with a size smaller than `word` will not be packed, so a `DynArray(byte)` will waste a lot of space
// TODO: storage representation
enum DynArray<t> {}

// Layout: the length lives at `loc`, so element i lives at `loc + 32 + i*32`.
// An index is in bounds when i < length.
impl<t> IndexAccess<memory<DynArray<t>>, t> where t: Typedef<word> {
    function get(ptr : memory<DynArray<t>>, i : uint256) returns (t) {
        let i_: word = Typedef.rep(i);
        let loc : word = Typedef.rep(ptr);
        if (i_ >= mload(loc)) { out_of_bounds(); }
        return Typedef.abs(mload(loc + 32 + (i_ * 32)));
    }
    function set(arr : memory<DynArray<t>>, i : uint256, val : t) returns (()) {
        let i_ : word = Typedef.rep(i);
        let loc : word = Typedef.rep(arr);
        if (i_ >= mload(loc)) { out_of_bounds(); }
        mstore(loc + 32 + (i_ * 32), Typedef.rep(val));
    }
}

// --- Array literals ---
//
// `[e1, ..., en]` is desugared, after type checking, into
//   arrayLitInit(... arrayLitInit(arrayLitNew(n), 0, e1) ..., n-1, en)
// The chain is a plain expression: each step returns the array it wrote to.

function arrayLitNew<t>(n : uint256) returns (memory<DynArray<t>>)  where t: Typedef<word> {
    let prx : Proxy<t>;
    return allocateDynamicArray(prx, Typedef.rep(n));
}

function arrayLitInit<t>(arr : memory<DynArray<t>>, i : uint256, v : t) returns (memory<DynArray<t>>)  where t: Typedef<word> {
    IndexAccess.set(arr, i, v);
    return arr;
}

function allocateDynamicArray<t>(prx : Proxy<t>, length : word) returns (memory<DynArray<t>>) {
    // size of allocation in bytes
    let sz : word = (length + 1) * 32;

    // get start of array & increment free by sz
    let free : word = get_free_memory();
    set_free_memory(free + sz);

    // write array length and return
    mstore(free, length);
    let res : memory<DynArray<t>> = Typedef.abs(free);
    return res;
}

// --- bytes ---

// tightly packed byte arrays
// bytes does not have a runtime representation since it can only ever exist in
// memory / calldata / storage and serves only as a type tag for pointer types
// TODO: IndexAccess for memory(bytes)
// TODO: IndexAccess for calldata(bytes)
// TODO: IndexAccess for storage(bytes)
enum bytes {}

// --- strings ---

// TODO: should this be a typedef over `bytes`?
enum string {}

impl Add<string> {
    function add(l: string, r: string) returns (string) {
        return concatLit(l, r);
    }
}

// ------------------------------------------------------------------
// Compile-time string literal builtins
// ------------------------------------------------------------------
// These are intended to be folded by MastEval when their arguments are
// statically known string literals.

function concatLit(comptime a: string, comptime b: string) returns (string) {
  unimplemented(); // Sanity check if folding ignores it.
  return "";
}

function strlenLit(comptime a: string) returns (word) {
  unimplemented(); // Sanity check if folding ignores it.
  return 0;
}

// Keccak-256 hash of the string-literal as UTF-8 bytes.
function keccakLit(comptime a: string) returns (word) {
  unimplemented(); // Sanity check if folding ignores it.
  return 0;
}

// Keccak-256 hash of a word's 32-byte big-endian representation.
// NOTE: this could be deprecated if we have comptime `to_bytes`.
function keccakWordLit(comptime a: word) returns (word) {
  unimplemented(); // Sanity check if folding ignores it.
  return 0;
}

// --- slices (sized pointers) ---

// A slice is a wrapper around an existing pointer type that extends the
// underlying type with information about the size of the data pointed to by `t`
enum slice<ptr> { slice(ptr, word) }

// --- Word Reader ---

// A WordReader is an abstraction over byte indexed structure that can be read in word sized chunks (e.g. calldata / memory)
// These let us use the same abi decoding routines for calldata / memory
trait WordReader<ty> {
    // returns the word currently pointed to by the WordReader
    function read(reader:ty) returns (word);
    // returns a new WordReader that points to a location `offset` bytes further into the array
    function advance(reader:ty, offset:word) returns (ty);
    // copies a block from the underlying source to memory
    function copyToMem(reader:ty, dst: word, cnt: word) returns (());
}

// WordReader for memory
enum MemoryWordReader { MemoryWordReader(word) }
impl WordReader<MemoryWordReader> {
    function read(reader:MemoryWordReader) returns (word) {
        match (reader ) {
        case MemoryWordReader(ptr) { return mload(ptr);
        } }
    }
    function advance(reader:MemoryWordReader, offset:word) returns (MemoryWordReader) {
        match (reader ) {
        case MemoryWordReader(ptr) { return MemoryWordReader(ptr + offset);
        } }
    }
    function copyToMem(reader:MemoryWordReader, dst:word, cnt: word) returns (()) {
        match (reader ) {
        case MemoryWordReader(ptr) { mcopy(dst, ptr, cnt);
        } }
    }
}

// WordReader for calldata
enum CalldataWordReader { CalldataWordReader(word) }

impl Typedef<CalldataWordReader, word> {
  function abs(a:word) returns (CalldataWordReader) { return CalldataWordReader(a); }
  function rep(r:CalldataWordReader) returns (word) {
    match (r ) {
      case CalldataWordReader(a) { return a;
    } }
  }
}

impl WordReader<CalldataWordReader> {
    function read(reader:CalldataWordReader) returns (word) {
        match (reader ) {
          case CalldataWordReader(ptr) { return calldataload(ptr);
        } }
    }
    function advance(reader:CalldataWordReader, offset:word) returns (CalldataWordReader) {
        match (reader ) {
        case CalldataWordReader(ptr) { return CalldataWordReader(ptr + offset);
        } }
    }
    function copyToMem(reader:CalldataWordReader, dst:word, cnt: word) returns (()) {
        match (reader ) {
        case CalldataWordReader(ptr) { calldatacopy(dst, ptr, cnt);
        } }
    }
}

// --- HasWordReader ---

// The HasWordReader class defines the types for which a WordReader can be produced
// We define instances for memory(bytes) and calldata(bytes)
trait HasWordReader<self, reader> {
    function getWordReader(x:self) returns (reader);
}

impl HasWordReader<memory<bytes>, MemoryWordReader> {
    function getWordReader(x:memory<bytes>) returns (MemoryWordReader) {
        return MemoryWordReader(Typedef.rep(x));
    }
}

impl HasWordReader<calldata<bytes>, CalldataWordReader> {
    function getWordReader(x:calldata<bytes>) returns (CalldataWordReader) {
        return CalldataWordReader(Typedef.rep(x));
    }
}

// --- MemoryType ---

// A MemoryType instance abstracts over type specific logic related to memory
// layout, allowing us to write code that is generic over which type is held in memory
trait MemoryType<self, loadedType> {
    // Proxy needed becaused class methods must mention strong type params
    // loads an instance of `loadedType` from an instance of `self` located at `loc` in memory
    function loadFromMemory(p:Proxy<self>, loc:word) returns (loadedType);
}

// A uint256 can be loaded from memory and pushed straight onto the stack
impl MemoryType<uint256, uint256> {
    function loadFromMemory(p:Proxy<uint256>, loc:word) returns (uint256) {
        return uint256(mload(loc));
    }
}

// We load a DynArray into a sized pointer to the first element
/*
forall ty ret . ty:MemoryType(ret) => instance DynArray(ty):MemoryType(slice(memory(ret))) {
    function loadFromMemory(p : Proxy (DynArray(ty)), loc:word) -> slice(memory(ret)) {
        let length = mload(loc);
        return slice(Typedef.abs(loc) : memory(ret), length);
    }
}
*/

// FAIL: patterson
// FAIL: bound variable
// if we ty is a MemoryType that returns deref and deref is ABIEncode, then we can encode a memory(ty)
// by loading it and then running the ABI encoding for the loaded value
/*
forall ty deref . ty:MemoryType(deref), deref:ABIEncode => instance memory(ty):ABIEncode {
    function encodeInto(x:memory(ty), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(ty); // FIXED: before was Proxy(deref)
        return ABIEncode.encodeInto(MemoryType.loadFromMemory(prx, Typedef.rep(x)) : deref, basePtr, offset, tail);
    }
}
*/
// --- ABI Tuples ---

// Tuples in Solidity are always desugared to nested pairs (to allow for
// inductive typeclass instance constructions) .
// This is an issue for the ABI routines since the ABI spec differentiates
// between `(1,1,1)` and `(1,(1,1))`, but the language treats both identically.
// The ABITuple type lets us reiintroduce this distinction:
// `ABITuple((1,(1,1))` should be treated as `(1,1,1)`  for the purposes of ABI
// encoding / decoding.
enum ABITuple<tuple> { ABITuple(tuple) }

impl<t> Typedef<ABITuple<t>, t> {
    function abs(t: t) returns (ABITuple<t>) {
        return ABITuple(t);
    }

    function rep(x: ABITuple<t>) returns (t) {
        match (x ) {
        case ABITuple(v) { return v;
        } }
    }
}

// --- ABI Metadata ---

// Statically knowable ABI related metadata about `self`
trait ABIAttribs<self> {
    // how many bytes should be used for the head portion of the abi encoding of `self`
    function headSize(ty:Proxy<self>) returns (word);
    // whether or not `self` is a fully static type
    function isStatic(ty:Proxy<self>) returns (bool);
}

default impl<t> ABIAttribs<t> {
    function headSize(ty : Proxy<t>) returns (word) { return 32; }
    function isStatic(ty : Proxy<t>) returns (bool) { return true; }
}

impl ABIAttribs<()> {
    function headSize(ty : Proxy<()>) returns (word) { return 0; }
    function isStatic(ty : Proxy<()>) returns (bool) { return true; }
}
impl ABIAttribs<uint256> {
    function headSize(ty : Proxy<uint256>) returns (word) { return 32; }
    function isStatic(ty : Proxy<uint256>) returns (bool) { return true; }
}
impl ABIAttribs<address> {
    function headSize(ty : Proxy<address>) returns (word) { return 32; }
    function isStatic(ty : Proxy<address>) returns (bool) { return true; }
}
impl<t> ABIAttribs<DynArray<t>> {
    function headSize(ty : Proxy<DynArray<t>>) returns (word) { return 32; }
    function isStatic(ty : Proxy<DynArray<t>>) returns (bool) { return false; }
}
// A dynamic array is encoded head-first as a 32-byte offset into the tail, so
// its head is one word and it is never static (matching DynArray above). This
// covers `array(t)` under any location qualifier via the `calldata(ty)` /
// `memory(ty)` ABIAttribs bridges.
impl<t> ABIAttribs<array<t>> {
    function headSize(ty : Proxy<array<t>>) returns (word) { return 32; }
    function isStatic(ty : Proxy<array<t>>) returns (bool) { return false; }
}
impl ABIAttribs<string> {
    function headSize(ty: Proxy<string>) returns (word) { return 32; }
    function isStatic(ty : Proxy<string>) returns (bool) { return false; }
}
// bytes is dynamic, exactly like string — without this instance it falls to the
// default (isStatic = true), which wrongly marks memory(bytes) (and any ADT
// carrying it) static, so calldata arrays/sums take the inline decode path over
// what is really an offset-referenced value.
impl ABIAttribs<bytes> {
    function headSize(ty: Proxy<bytes>) returns (word) { return 32; }
    function isStatic(ty : Proxy<bytes>) returns (bool) { return false; }
}

// computes the attribs for a pair of two types that implement attribs
impl<a, b> ABIAttribs<(a, b)> where a: ABIAttribs, b: ABIAttribs {
    function headSize(ty : Proxy<(a, b)>) returns (word) {
        let pa : Proxy<a>;
        let pb : Proxy<b>;
        let sza = ABIAttribs.headSize(pa);
        let szb = ABIAttribs.headSize(pb);
        return sza + szb;
    }
    function isStatic(ty : Proxy<(a, b)>) returns (bool) {
        let pa : Proxy<a>;
        let pb : Proxy<b>;
        return and(ABIAttribs.isStatic(pa), ABIAttribs.isStatic(pb));
    }
}

// if an abi tuple contains dynamic elems we store it in the tail, otherwise we
// treat it the same as a series of nested pairs
impl<tuple> ABIAttribs<ABITuple<tuple>> where tuple: ABIAttribs {
    function headSize(ty : Proxy<ABITuple<tuple>>) returns (word) {
        let px : Proxy<tuple>;
        match (ABIAttribs.isStatic(px) ) {
        case true { return ABIAttribs.headSize(px);
        } case false { return 32;
        } }
    }
    function isStatic(ty : Proxy<ABITuple<tuple>>) returns (bool) {
        let px : Proxy<tuple>;
        return ABIAttribs.isStatic(px);
    }
}

// for pointer types we fetch the attribs of the pointed to type, not the pointer itself
impl<ty> ABIAttribs<memory<ty>> where ty: ABIAttribs {
    function headSize(p : Proxy<memory<ty>>) returns (word) {
        let px : Proxy<ty>;
        return ABIAttribs.headSize(px);
    }
    function isStatic(p : Proxy<memory<ty>>) returns (bool) {
        let px : Proxy<ty>;
        return ABIAttribs.isStatic(px);
    }
}
impl<ty> ABIAttribs<calldata<ty>> where ty: ABIAttribs {
    function headSize(p : Proxy<calldata<ty>>) returns (word) {
        let px : Proxy<ty>;
        return ABIAttribs.headSize(px);
    }
    function isStatic(ty : Proxy<calldata<ty>>) returns (bool) {
        let px : Proxy<ty>;
        return ABIAttribs.isStatic(px);
    }
}

// --- ABI Encoding ---
// TODO: make these generic over the location being written to (i.e. memory or returndata)

// top level encoding function.
// abi encodes an instance of `ty` and returns a pointer to the result
function abi_encode<ty>(val : ty) returns (memory<bytes>)  where ty: ABIAttribs, ty: ABIEncode {
    let ret = get_free_memory();
    let start = ret + 32;
    let tail = ABIEncode.encodeInto(val, start, 0, start + ABIAttribs.headSize(@ty));
    mstore(ret, tail - start);
    set_free_memory(tail);
    return memory(ret);
}

// types that can be abi encoded
trait ABIEncode<self> {
    // abi encodes an instance of self into a memory region starting at basePtr
    // offset gives the offset in memory from basePtr to the first empty byte of the head
    // tail gives the index in memory of the first empty byte of the tail
    function encodeInto(x:self, basePtr:word, offset:word, tail:word) returns (word) /* newTail */;
}

impl ABIEncode<uint256> {
    // a unit256 is written directly into the head
    function encodeInto(x:uint256, basePtr:word, offset:word, tail:word) returns (word) {
        let repx : word = Typedef.rep(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}

impl ABIEncode<address> {
    // an address is written directly into the head (into a full 32-byte slot)
    function encodeInto(x:address, basePtr:word, offset:word, tail:word) returns (word) {
        let repx : word = Typedef.rep(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}

impl ABIEncode<bytes32> {
    // a bytes32 is written directly into the head
    function encodeInto(x:bytes32, basePtr:word, offset:word, tail:word) returns (word) {
        let repx : word = Typedef.rep(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}

impl ABIEncode<bytes4> {
    // bytes4's word rep is right-aligned (e.g. `bytes4(shr(224, h))`),
    // so it is written directly into the head like bytes32
    function encodeInto(x:bytes4, basePtr:word, offset:word, tail:word) returns (word) {
        let repx : word = Typedef.rep(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}

impl ABIEncode<bool> {
    function encodeInto(x:bool, basePtr:word, offset:word, tail:word) returns (word) {
        let repx : word = frombool(x);
        mstore(basePtr + offset, repx);
        return tail;
    }
}

function round_up_to_mul_of_32(value:word) returns (word) {
    return (value + 31) & ~31;
}

function encodeIntoFromBytesLike(srcPtr:word, basePtr:word, offset:word, tail:word) returns (word) {
    let length = mload(srcPtr);
    let total = length + 32;
    mstore(basePtr + offset, tail - basePtr);
    mcopy(tail, srcPtr, total);
    let rounded = round_up_to_mul_of_32(total);
    zeroize_memory(tail + total, rounded - total);
    return tail + rounded;
}

impl ABIEncode<memory<string>> {
    function encodeInto(x:memory<string>, basePtr:word, offset:word, tail:word) returns (word) {
      return encodeIntoFromBytesLike(Typedef.rep(x), basePtr, offset, tail);
    }
}

impl ABIEncode<memory<bytes>> {
    function encodeInto(x:memory<bytes>, basePtr:word, offset:word, tail:word) returns (word) {
      return encodeIntoFromBytesLike(Typedef.rep(x), basePtr, offset, tail);
    }
}

// ABI encoding for a memory dynamic array whose elements fit in a single word.
// Assumes memory layout `[ length | elem_0 | elem_1 | ... ]`, which matches the
// on-the-wire tail of `t[]` so the body can be `mcopy`d verbatim.
// `memory(DynArray(t)):ABIAttribs` is already derivable from the generic
// `memory(ty):ABIAttribs` + `DynArray(t):ABIAttribs` instances above.
impl<t> ABIEncode<memory<DynArray<t>>> where t: Typedef<word> {
    function encodeInto(x:memory<DynArray<t>>, basePtr:word, offset:word, tail:word) returns (word) {
        let srcPtr : word = Typedef.rep(x);
        let len : word = mload(srcPtr);
        let totalBytes : word = (len + 1) * 32;

        // head slot: relative pointer from basePtr to tail
        mstore(basePtr + offset, tail - basePtr);

        // copy length + elements verbatim into the tail
        let s : word = srcPtr;
        let t_ : word = tail;
        let n : word = totalBytes;
        mcopy(t_, s, n);
        return tail + totalBytes;
    }
}

impl ABIEncode<()> {
    // a unit256 is written directly into the head
    function encodeInto(x:(), basePtr:word, offset:word, tail:word) returns (word) {
        return tail;
    }
}

// abi encoding for a pair of two encodable types
impl<a, b> ABIEncode<(a, b)> where a: ABIAttribs, a: ABIEncode, b: ABIEncode {
    function encodeInto(x: (a, b), basePtr: word, offset: word, tail: word) returns (word) {
        match (x ) {
        case (l,r) {
            let newTail = ABIEncode.encodeInto(l, basePtr, offset, tail);
            let pa : Proxy<a>;
            let a_sz = ABIAttribs.headSize(pa);
            return ABIEncode.encodeInto(r, basePtr, offset + a_sz, newTail);
        } }
    }
}


// abi encoding for an ABITuple of encodable types
// TODO: is this correct?
impl<tuple> ABIEncode<ABITuple<tuple>> where tuple: ABIEncode, tuple: ABIAttribs {
    function encodeInto(x:ABITuple<tuple>, basePtr:word, offset:word, tail:word) returns (word) {
        let prx : Proxy<tuple>;
        match (ABIAttribs.isStatic(prx) ) {
        // if the tuple contains only static elements then we encode it in the head
        case true { return ABIEncode.encodeInto(Typedef.rep(x), basePtr, offset, tail);
        // if the tuple contains dynamically sized elements then we store a
        // pointer in the head, and encode the tuple into the tail
        } case false {
            // store the length of the head in basePtr
            mstore(basePtr, tail - basePtr);

            // encode the underlying tuple into the tail
            let headSize = ABIAttribs.headSize(@tuple);
            basePtr = tail;
            tail += headSize;
            return ABIEncode.encodeInto(Typedef.rep(x), basePtr, 0, tail);
        } }
    }
}

// --- ABI Decoding ---

// Top level decoding function.
// abi decodes an instance of `decodable` into a `ty`
function abi_decode<decodable, reader, ty, decoded>(decodable:decodable, pty:Proxy<ty>, prdr:Proxy<reader>) returns (decoded)  where decodable: HasWordReader<reader>, ABIDecoder<ty, reader>: ABIDecode<decoded> {
    let decoder : ABIDecoder<ty, reader> = ABIDecoder(HasWordReader.getWordReader(decodable));
    return ABIDecode.decode(decoder, 0);
}


trait ABIDecode<decoder, decoded> {
    function decode(ptr:decoder, currentHeadOffset:word) returns (decoded);
}

// An ABI Decoder for `ty` from `reader`
// This lets us abstract over memory and calldata when decoding
enum ABIDecoder<ty, reader> { ABIDecoder(reader) }

// If `reader` is a `WordReader` then so is our `ABIDecoder`
impl<ty, reader> WordReader<ABIDecoder<ty, reader>> where reader: WordReader {
    function read(decoder:ABIDecoder<ty, reader>) returns (word) {
        match (decoder ) {
        case ABIDecoder(ptr) { return WordReader.read(ptr);
        } }
    }
    function advance(decoder:ABIDecoder<ty, reader>, offset:word) returns (ABIDecoder<ty, reader>) {
        match (decoder ) {
        case ABIDecoder(ptr) { return ABIDecoder(WordReader.advance(ptr, offset));
        } }
    }
    function copyToMem(decoder:ABIDecoder<ty, reader>, dst:word, cnt: word) returns (()) {
        match (decoder ) {
        case ABIDecoder(ptr) { WordReader.copyToMem(ptr, dst, cnt);
        } }
    }
}

// ABI Decoding for uint256
impl<reader> ABIDecode<ABIDecoder<uint256, reader>, uint256> where reader: WordReader {
    function decode(ptr:ABIDecoder<uint256, reader>, currentHeadOffset:word) returns (uint256) {
        let syntaxValue1: uint256 = Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
        return syntaxValue1;
    }
}

// ABI Decoding for bytes32
impl<reader> ABIDecode<ABIDecoder<bytes32, reader>, bytes32> where reader: WordReader {
    function decode(ptr:ABIDecoder<bytes32, reader>, currentHeadOffset:word) returns (bytes32) {
        let syntaxValue2: bytes32 = Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
        return syntaxValue2;
    }
}

// ABI Decoding for bytes4
impl<reader> ABIDecode<ABIDecoder<bytes4, reader>, bytes4> where reader: WordReader {
    function decode(ptr:ABIDecoder<bytes4, reader>, currentHeadOffset:word) returns (bytes4) {
        let syntaxValue3: bytes4 = Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
        return syntaxValue3;
    }
}

// ABI Decoding for bool
// bool is a builtin (not a Typedef(word)), so it round-trips through word via
// tobool, mirroring the bool:ABIEncode instance which uses frombool.
impl<reader> ABIDecode<ABIDecoder<bool, reader>, bool> where reader: WordReader {
    function decode(ptr:ABIDecoder<bool, reader>, currentHeadOffset:word) returns (bool) {
        let v = WordReader.read(WordReader.advance(ptr, currentHeadOffset));
        require(v <= 1, Error(0x0557dbbf)); // DirtyHigherBitsForBool()
        return tobool(v);
    }
}

// ABI Decoding for address
impl<reader> ABIDecode<ABIDecoder<address, reader>, address> where reader: WordReader {
    function decode(ptr:ABIDecoder<address, reader>, currentHeadOffset:word) returns (address) {
        let raw = WordReader.read(WordReader.advance(ptr, currentHeadOffset));
        require(shr(160, raw) == 0, Error(0x7cc04fa7)); // DirtyHigherBitsForAddress()
        let syntaxValue4: address = Typedef.abs(raw);
        return syntaxValue4;
    }
}

impl<reader> ABIDecode<ABIDecoder<(), reader>, ()> where reader: WordReader {
    function decode(ptr:ABIDecoder<(), reader>, currentHeadOffset:word) returns (()) {
        return;
    }
}

// ABI decoding for bytes/strings (only in memory)
function decodeBytesLike<a, ptrtype, reader>(ptr:ABIDecoder<memory<a>, reader>, currentHeadOffset:word) returns (memory<a>)  where reader: WordReader {
        let tmp:word;
        let headRdr = WordReader.advance(ptr, currentHeadOffset);
        let tailPtr : word = WordReader.read(headRdr);

        let src = WordReader.advance(ptr, tailPtr);
        let srcRdr = getReader(src);
        let length = WordReader.read(src);
        let total = length + 32;
        let rounded = round_up_to_mul_of_32(total);
        let resultPtr : word = allocate_memory(rounded);
        WordReader.copyToMem(srcRdr, resultPtr, total);
        return memory(resultPtr);
}

// ABI decoding for strings (only in memory)
impl<reader> ABIDecode<ABIDecoder<memory<string>, reader>, memory<string>> where reader: WordReader {
    function decode(ptr:ABIDecoder<memory<string>, reader>, currentHeadOffset:word) returns (memory<string>) {
      return decodeBytesLike(ptr, currentHeadOffset);
    }
}

// ABI decoding for bytes (only in memory)
impl<reader> ABIDecode<ABIDecoder<memory<bytes>, reader>, memory<bytes>> where reader: WordReader {
    function decode(ptr:ABIDecoder<memory<bytes>, reader>, currentHeadOffset:word) returns (memory<bytes>) {
      return decodeBytesLike(ptr, currentHeadOffset);
    }
}

// ABI decoding for a pair of decodable values
// FAIL: Coverage
impl<a, b, a_decoded, b_decoded, reader> ABIDecode<ABIDecoder<(a, b), reader>, (a_decoded, b_decoded)> where reader: WordReader, ABIDecoder<b, reader>: ABIDecode<b_decoded>, ABIDecoder<a, reader>: ABIDecode<a_decoded>, a: ABIAttribs {
    function decode(ptr:ABIDecoder<(a, b), reader>, currentHeadOffset:word) returns ((a_decoded, b_decoded)) {
        match (ptr ) {
        case ABIDecoder(rdr) {
            let prx : Proxy<a>;
            let decoder_a : ABIDecoder<a, reader> = ABIDecoder(rdr);
            let decoder_b : ABIDecoder<b, reader> = ABIDecoder(rdr);
            let a_val : a_decoded = ABIDecode.decode(decoder_a, currentHeadOffset);
            let b_val : b_decoded = ABIDecode.decode(decoder_b, currentHeadOffset + ABIAttribs.headSize(prx));
            return (a_val, b_val);
        } }
    }
}

impl<reader, tuple, tuple_decoded> ABIDecode<ABIDecoder<ABITuple<tuple>, reader>, tuple_decoded> where reader: WordReader, tuple: ABIDecode<tuple_decoded>, tuple: ABIAttribs {
    function decode(ptr:ABIDecoder<ABITuple<tuple>, reader>, currentHeadOffset:word) returns (tuple_decoded) {
        let prx : Proxy<tuple>;
        match (ABIAttribs.isStatic(prx) ) {
        case true { return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        } case false {
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
       } }
    }
}


impl<reader, tuple, tuple_decoded> ABIDecode<ABIDecoder<memory<ABITuple<tuple>>, reader>, memory<tuple_decoded>> where reader: WordReader, tuple: ABIDecode<tuple_decoded>, tuple: ABIAttribs {
    function decode(ptr:ABIDecoder<memory<ABITuple<tuple>>, reader>, currentHeadOffset:word) returns (memory<tuple_decoded>) {
        let prx : Proxy<tuple>;
        match (ABIAttribs.isStatic(prx) ) {
        case true { return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        } case false {
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
        } }
    }
}

impl<reader, baseType, baseType_decoded> ABIDecode<ABIDecoder<memory<DynArray<baseType>>, reader>, memory<DynArray<baseType_decoded>>> where baseType: ABIAttribs, reader: WordReader, ABIDecoder<baseType, reader>: ABIDecode<baseType_decoded> {
    function decode(ptr:ABIDecoder<memory<DynArray<baseType>>, reader>, currentHeadOffset:word) returns (memory<DynArray<baseType_decoded>>) {
        let arrayPtr = WordReader.advance(ptr, currentHeadOffset);
        let length = WordReader.read(arrayPtr);
        // this trigger a missing typedef constraint
        // let elementPtr:ABIDecoder(baseType, reader) = Typedef.abs(WordReader.advance(arrayPtr, 32));
        arrayPtr = WordReader.advance(arrayPtr, 32);
        let prx : Proxy<baseType_decoded>;
        let result : memory<DynArray<baseType_decoded>> = allocateDynamicArray(prx, length);
        let offset : word = 0;
        let prx : Proxy<baseType>;
        let elementHeadSize : word = ABIAttribs.headSize(prx);

        // TODO: surface level loops
        // TODO: sugar for assigning to indexAccess types (result[i])
        //for(let i = 0; i < length; i++) {
            //result[i] = ABIDecode.decode(elementPtr, offset);
            //assembly { offset := add(offset, elementHeadSize) }
        //}

        return result;
    }
}

function getReader<ty, reader>(d:ABIDecoder<ty, reader>) returns (reader) {
    match (d ) {
      case ABIDecoder(rdr) { return rdr;
    } }
}

impl<baseType, baseType_decoded> ABIDecode<ABIDecoder<calldata<DynArray<baseType>>, CalldataWordReader>, calldata<DynArray<baseType_decoded>>> where ABIDecoder<baseType, CalldataWordReader>: ABIDecode<baseType_decoded>, baseType: WordReader {
     function decode(ptr:ABIDecoder<calldata<DynArray<baseType>>, CalldataWordReader>, currentHeadOffset:word) returns (calldata<DynArray<baseType_decoded>>) {
          let newptr = WordReader.advance(ptr, currentHeadOffset);
          let reader: CalldataWordReader = getReader(newptr);
          let addr: word = Typedef.rep(reader);
          return Typedef.abs(addr);
     }
 }

// ─── Lazy ABI decode of a calldata dynamic array ─────────────────────────────
// The head slot holds the (args-relative) byte offset to the array data;
// following it lands on the length word. The decoded value is a calldata handle
// to that length word, so the elements are left in calldata and decoded on
// demand (abiArrayLength / abiArrayGet). Because nothing is materialised here,
// this works for any decodable element type — including multi-word ADTs such as
// a sum(...) — which the word-per-slot memory(DynArray(...)) path cannot hold.
impl<baseType, baseType_decoded> ABIDecode<ABIDecoder<calldata<array<baseType>>, CalldataWordReader>, calldata<array<baseType_decoded>>> where ABIDecoder<baseType, CalldataWordReader>: ABIDecode<baseType_decoded> {
     function decode(ptr:ABIDecoder<calldata<array<baseType>>, CalldataWordReader>, currentHeadOffset:word) returns (calldata<array<baseType_decoded>>) {
          let headRdr = WordReader.advance(ptr, currentHeadOffset);
          let dataOffset : word = WordReader.read(headRdr);
          let dataRdr = WordReader.advance(ptr, dataOffset);
          let rdr : CalldataWordReader = getReader(dataRdr);
          let addr : word = Typedef.rep(rdr);
          return Typedef.abs(addr);
     }
 }

// Length of a decoded calldata array: the handle points at the length word.
function abiArrayLength<t>(a : calldata<array<t>>) returns (uint256) {
    let rdr : CalldataWordReader = CalldataWordReader(Typedef.rep(a));
    return uint256(WordReader.read(rdr));
}

// Decode element `i` of a calldata array on demand. The element region starts
// one word after the handle (past the length word). Two layouts, per the ABI:
//
//   * static element type  -> elements sit inline, each headSize(t) bytes, so
//     element i starts at (handle + 32) + i * headSize(t). The element decoder
//     is aimed at the region base and the per-element offset is threaded as the
//     head offset.
//
//   * dynamic element type -> the region holds a table of 32-byte offsets (one
//     per element, relative to the region base), each pointing at that
//     element's own encoding (standard-ABI T[] for dynamic T). The element
//     decoder is aimed at the region base and given element i's slot as its
//     head offset; the element's own dynamic decoder follows that offset. This
//     is uniform across element kinds: a dynamic sum follows it and rebases to
//     the element start, a bare bytes/string leaf follows it to its length word.
function abiArrayGet<t, t_decoded>(a : calldata<array<t>>, i : uint256) returns (t_decoded)  where t: ABIAttribs, ABIDecoder<t, CalldataWordReader>: ABIDecode<t_decoded> {
    // Bounds check: valid indices are [0, length); i == length is already past
    // the last element, so reject i >= length (mirrors the storage-array guard).
    require(i < abiArrayLength(a), Error(0x7f52b2bf)); // ArrayOutOfBounds()
    let base : word = Typedef.rep(a);
    let elemRegion : word = base + 32;
    let prx : Proxy<t>;
    let idx : word = Typedef.rep(i);
    match (ABIAttribs.isStatic(prx) ) {
    case true {
        let elemRdr : CalldataWordReader = CalldataWordReader(elemRegion);
        let dec : ABIDecoder<t, CalldataWordReader> = ABIDecoder(elemRdr);
        return ABIDecode.decode(dec, idx * ABIAttribs.headSize(prx));
    } case false {
        // Dynamic elements: the region is a table of 32-byte offsets (relative
        // to the region base), one per element. Hand the element decoder the
        // region base and element i's slot as its head offset; the element's own
        // (dynamic) decoder follows that offset — uniformly for a dynamic sum
        // element or a bare bytes/string element (calldata(array(bytes))).
        let elemRdr : CalldataWordReader = CalldataWordReader(elemRegion);
        let dec : ABIDecoder<t, CalldataWordReader> = ABIDecoder(elemRdr);
        return ABIDecode.decode(dec, idx * 32);
    } }
}


// --- Assignment ---

/*
# Types and classes for assignemnt desugaring
- access proxy types
- LValue and RValue access classes (LVA, RVA)
- Assign class
*/


pragma no-patterson-condition RVA, Assign;
pragma no-coverage-condition MemberAccessProxy, LVA, RVA, CStructField, Assign;
pragma no-bounded-variable-condition LVA, RVA;

// --- Storage ---

// Zeroes the storage slots in [start, endSlot). Mirrors solc's
// clear_storage_range, used when a dynamic array shrinks so that regrowing it
// cannot resurrect the old elements.
function clearStorageRange(start: word, endSlot: word) returns (()) {
    for (; start < endSlot; start += 1) {
        sstore(start, 0);
    }
}

trait StorageSize<self> {
    function size(x:Proxy<self>) returns (word);
}


default impl<self> StorageSize<self> {
    function size(x:Proxy<self>) returns (word) {
        return 1;
    }
}

impl StorageSize<()> {
    function size(x:Proxy<()>) returns (word) {
        return 0;
    }
}

impl StorageSize<word> {
    function size(x:Proxy<word>) returns (word) {
        return 1;
    }
}
/*
instance uint:StorageSize {
    function size(x:Proxy(uint)) -> word {
        return 1;
    }
}
*/
impl StorageSize<uint256> {
    function size(x:Proxy<uint256>) returns (word) {
        return 1;
    }
}

impl StorageSize<bytes32> {
    function size(x:Proxy<bytes32>) returns (word) {
        return 1;
    }
}

impl StorageSize<address> {
    function size(x:Proxy<address>) returns (word) {
        return 1;
    }
}

impl StorageSize<string> {
    function size(x:Proxy<string>) returns (word) {
        return 1;
    }
}

impl StorageSize<memory<string>> {
    function size(x:Proxy<memory<string>>) returns (word) {
        return 1;
    }
}

impl StorageSize<bytes> {
    function size(x:Proxy<bytes>) returns (word) {
        return 1;
    }
}

impl StorageSize<memory<bytes>> {
    function size(x:Proxy<memory<bytes>>) returns (word) {
        return 1;
    }
}

impl<a, b> StorageSize<(a, b)> where a: StorageSize, b: StorageSize {
    function size(x:Proxy<(a, b)>) returns (word) {
        let a_sz:word = StorageSize.size(@a);
        let b_sz:word = StorageSize.size(@b);
        return a_sz + b_sz;
    }
}

trait StorageType<self> {
    function load(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
}

// How to copy one element of type self from one storage slot to another.
// Whole-array assignment (a = b) copies element by element through this class,
// the way solc's copy_array_to_storage calls the element's own copy routine.
// The constraint lives on the *element* type, so it can gate CanStore.store for
// storage(array(self)) without also gating CanStore.load, which must stay
// unconstrained, a field read has to yield the array's storage reference.
// Instances live below, next to the CanStore instances the dynamic ones rely on.
trait StorageCopy<self> {
    function copySlot(dst:storage<self>, src:storage<self>) returns (());
}

impl StorageType<word> {
    function load(ptr:word) returns (word) {
        return sload(ptr);
    }
    function store(ptr:word, value:word) returns (()) {
        sstore(ptr, value);
    }
}

impl StorageType<uint256> {
  function load(ptr:word) returns (uint256) { let syntaxValue5: word = StorageType.load(ptr); return uint256(syntaxValue5); }
  function store(ptr:word, value:uint256) returns (()) { let syntaxValue6: word = Typedef.rep(value); StorageType.store(ptr, syntaxValue6); }
}

impl StorageType<bytes32> {
  function load(ptr:word) returns (bytes32) { let syntaxValue7: word = StorageType.load(ptr); return bytes32(syntaxValue7); }
  function store(ptr:word, value:bytes32) returns (()) { let syntaxValue8: word = Typedef.rep(value); StorageType.store(ptr, syntaxValue8); }
}

impl StorageType<address> {
  function load(ptr:word) returns (address) { let syntaxValue9: word = StorageType.load(ptr); return address(syntaxValue9); }
  function store(ptr:word, value:address) returns (()) { let syntaxValue10: word = Typedef.rep(value); StorageType.store(ptr, syntaxValue10); }
}

// -- structure fields (including contract fields)

trait CStructField<self, fieldType, offsetType> {}
enum StructField<structType, fieldSelector> { StructField(structType) }


enum MemberAccessProxy<a, field, fieldtype, offset> { MemberAccessProxy(a, field) }

function memberAccessBase<a, field, fieldType, storageType, offset>(x:MemberAccessProxy<a, field, fieldType, offset>) returns (a) {
    match (x ) {
        case MemberAccessProxy(y,z) { return y;
    } }
}


// ------------------------------------------------------------------
// Contract field access
// ------------------------------------------------------------------

impl<cxt, fieldSelector, loadType, offsetType, storageType> LVA<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, loadType, offsetType>, storage<storageType>> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<storage<storageType>, offsetType>, offsetType: StorageSize, storage<storageType>: CanStore<loadType> {
   function acc (x : MemberAccessProxy<ContractStorage<cxt>, fieldSelector, loadType, offsetType>) returns (storage<storageType>) {
      let offset : word = StorageSize.size(@offsetType) ;
      let syntaxValue11: storage<storageType> = storage(offset);
      return syntaxValue11;
   }
}

impl<cxt, fieldSelector, loadType, offsetType, storageType> RVA<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, loadType, offsetType>, loadType> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<storage<storageType>, offsetType>, storage<storageType>: CanStore<loadType>, offsetType: StorageSize {
    function acc(x:MemberAccessProxy<ContractStorage<cxt>, fieldSelector, loadType, offsetType>) returns (loadType) {
        let offset:word = StorageSize.size(@offsetType);
        let syntaxValue13: storage<storageType> = storage(offset);
        let syntaxValue12: loadType = CanStore.load(syntaxValue13);
        return syntaxValue12;
    }
}

// TODO: structures other than contract context
/*
forall structType fieldSelector fieldType storageType offsetType
  . StructField(structType, fieldSelector):CStructField(fieldType, offsetType)
  , offsetType:StorageSize
  => instance MemberAccessProxy(storage(structType), fieldSelector, fieldType,  offsetType):LVA(storage(fieldType)) {
    function acc(x:MemberAccessProxy(storage(structType), fieldSelector, fieldType,  offsetType)) -> storage(fieldType) {
        let ptr:word = Typedef.rep(memberAccessBase(x));
        let size:word = StorageSize.size(Proxy:Proxy(offsetType));
        return storage(ptr + size);
    }
}

forall structType fieldSelector fieldType storageType offsetType
  . StructField(structType, fieldSelector):CStructField(fieldType, offsetType)
  , offsetType:StorageSize
  , fieldType:StorageType
  => instance MemberAccessProxy(storage(structType), fieldSelector, fieldType,  offsetType):RVA(fieldType) {
    function acc(x:MemberAccessProxy(storage(structType), fieldSelector, fieldType,  offsetType)) -> fieldType {
        let ptr:word = Typedef.rep(memberAccessBase(x));
        let size:word = StorageSize.size(Proxy:Proxy(offsetType));
        return CanStore.load(ptr + size);
    }
}
*/



enum ContractStorage<cxt> { ContractStorage(cxt) }


impl<member, index> Typedef<mapping(index => member), word> {
    function rep(x:mapping(index => member)) returns (word) {
        match (x ) {
            case mapping(y) { return y;
        } }
    }
    function abs(x:word) returns (mapping(index => member)) {
        return mapping(x);
    }
}


// cf https://docs.soliditylang.org/en/latest/internals/layout_in_storage.html#mappings-and-dynamic-arrays
impl<index, member> StorageSize<mapping(index => member)> {
    function size(x:Proxy<mapping(index => member)>) returns (word) {
        return 1;
    }
}

impl<member> Typedef<array<member>, word> {
    function rep(x:array<member>) returns (word) {
        match (x ) {
            case array(y) { return y;
        } }
    }
    function abs(x:word) returns (array<member>) {
        return array(x);
    }
}

// cf https://docs.soliditylang.org/en/latest/internals/layout_in_storage.html#mappings-and-dynamic-arrays
// the slot itself stores the array length; elements live at keccak256(slot) + i
impl<member> StorageSize<array<member>> {
    function size(x:Proxy<array<member>>) returns (word) {
        return 1;
    }
}

trait Length<self> {
    function length(arr:self) returns (uint256);
}

// Dynamic storage arrays carry their length at the slot itself (matching the
// Solidity convention) while elements live at keccak256(slot) + i.
trait Array<self> {
    function setLength(arr:self, n:uint256) returns (());
    function pop(arr:self) returns (());
}

// push is split into its own MPTC so its element type only shows up where it
// actually matters (the value being appended), without forcing `length`/
// `setLength`/`pop` to drag along an unconstrained `elem` parameter.
trait ArrayPush<self, elem> {
    function push(arr:self, val:elem) returns (());
}

impl<t> Length<storage<array<t>>> {
    function length(arr:storage<array<t>>) returns (uint256) {
        return uint256(sload(Typedef.rep(arr)));
    }
}

// A lazily-decoded calldata array reports its length from the head length-word
// of its handle (see abiArrayLength), so `arr.length()` resolves through the
// same Length class / UFCS as storage arrays.
impl<t> Length<calldata<array<t>>> {
    function length(arr:calldata<array<t>>) returns (uint256) {
        return abiArrayLength(arr);
    }
}

impl<t> Array<storage<array<t>>> {
    // Shrinking clears the abandoned slots, matching solc's resize_array.
    // For string/bytes elements this zeroes the inline slot, which makes any
    // keccak-derived tail unreachable (reads are governed by the length word) but
    // does not reclaim it.
    function setLength(arr:storage<array<t>>, n:uint256) returns (()) {
        let slot : word = Typedef.rep(arr);
        let oldLen : word = sload(slot);
        let newLen : word = Typedef.rep(n);
        if (newLen < oldLen) {
            let base : word = hash1(slot);
            clearStorageRange(base + newLen, base + oldLen);
        }
        sstore(slot, newLen);
    }
    // Zeroes the removed element before decrementing, as solc's array_pop does.
    function pop(arr:storage<array<t>>) returns (()) {
        let slot : word = Typedef.rep(arr);
        let n : word = sload(slot);
        if (n == 0) { out_of_bounds(); }
        sstore(hash1(slot) + (n - 1), 0);
        sstore(slot, n - 1);
    }
}

// The value pushed is whatever the element's storage reference can store, rather
// than the element tag type itself. That is what lets array(string) accept a
// memory(string), via storage(string):CanStore(memory(string)). For word-sized
// elements v collapses to the element type and CanStore.store delegates to
// StorageType.store, so the generated code is unchanged.
impl<t, v> ArrayPush<storage<array<t>>, v> where storage<t>: CanStore<v> {
    function push(arr:storage<array<t>>, val:v) returns (()) {
        let slot : word = Typedef.rep(arr);
        let n : word = sload(slot);
        let syntaxValue14: storage<t> = storage(hash1(slot) + n);
        CanStore.store(syntaxValue14, val);
        sstore(slot, n + 1);
    }
}

trait LVA<self, memberRefType> {
    function acc(x:self) returns (memberRefType);
}


trait RVA<self, member> {
    function acc(x:self) returns (member);
}

function rval<a, b>(x:a) returns (b)  where a: RVA<b> {
  return RVA.acc(x);
}


// TODO: consider merging CanStore and Assign
trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}


// a can store b; e.g. storage(string) : memory(string)
trait CanStore<a, b> {
  function store(r:a, v:b) returns (());
  function load(r:a) returns (b);
}


impl<a, b> Assign<a, b> where a: CanStore<b> {
    function assign(l:a, r:b) returns (()) {
      CanStore.store(l, r);
    }
}

/*
forall a. a:StorageType =>
default instance a:CanStore(a) {
    function store(l:storage(a), r:a) -> () {
      StorageType.store(Typedef.rep(l), r);
    }
    function load(l:storage(a)) -> a {
      return StorageType.load(Typedef.rep(l));
    }
}
*/

 impl CanStore<storage<word>, word> {
    function store(l:storage<word>, r:word) returns (()) {
      StorageType.store(Typedef.rep(l), r);
    }
    function load(l:storage<word>) returns (word) {
      return StorageType.load(Typedef.rep(l));
    }
}

 impl CanStore<storage<uint256>, uint256> {
    function store(l:storage<uint256>, r:uint256) returns (()) {
      StorageType.store(Typedef.rep(l), r);
    }
    function load(l:storage<uint256>) returns (uint256) {
      return StorageType.load(Typedef.rep(l));
    }
}

 impl CanStore<storage<bytes32>, bytes32> {
    function store(l:storage<bytes32>, r:bytes32) returns (()) {
      StorageType.store(Typedef.rep(l), r);
    }
    function load(l:storage<bytes32>) returns (bytes32) {
      return StorageType.load(Typedef.rep(l));
    }
}

 impl CanStore<storage<address>, address> {
    function store(l:storage<address>, r:address) returns (()) {
      StorageType.store(Typedef.rep(l), r);
    }
    function load(l:storage<address>) returns (address) {
      return StorageType.load(Typedef.rep(l));
    }
}

// bool has no StorageType instance (it is a builtin, not a Typedef(word)), but it
// round-trips through word via frombool / tobool, so it can still be stored.
impl CanStore<storage<bool>, bool> {
    function store(l:storage<bool>, r:bool) returns (()) {
      StorageType.store(Typedef.rep(l), frombool(r));
    }
    function load(l:storage<bool>) returns (bool) {
      return tobool(StorageType.load(Typedef.rep(l)));
    }
}

impl<k, v> CanStore<storage<mapping(k => v)>, storage<mapping(k => v)>> {
    function store(l:storage<mapping(k => v)>, r:storage<mapping(k => v)>) returns (()) {
      // StorageType.store(Typedef.rep(l), r);
      unimplemented();
    }
    function load(l:storage<mapping(k => v)>) returns (storage<mapping(k => v)>) {
      // "Loading" a storage mapping field yields its storage reference (the
      // slot); indexed access / method calls consume that reference directly.
      return l;
    }
}

impl<v> CanStore<storage<array<v>>, storage<array<v>>> where v: StorageCopy {
    // Whole-array assignment is a deep copy, as in Solidity: a = b resizes a
    // to b's length and then copies every
    // element. Assigning an array to itself is a no-op. A *local* bound to an
    // array field stays an alias, because a let is not an Assign.assign.
    function store(l:storage<array<v>>, r:storage<array<v>>) returns (()) {
      let dst : word = Typedef.rep(l);
      let src : word = Typedef.rep(r);
      if (dst != src) {
        let oldLen : word = sload(dst);
        let newLen : word = sload(src);
        let dstBase : word = hash1(dst);
        if (newLen < oldLen) {
          clearStorageRange(dstBase + newLen, dstBase + oldLen);
        }
        sstore(dst, newLen);
        let srcBase : word = hash1(src);
        for (let i = 0; i < newLen; i += 1) {
          let syntaxValue15: storage<v> = storage(dstBase + i);
          let syntaxValue16: storage<v> = storage(srcBase + i);
          StorageCopy.copySlot(syntaxValue15, syntaxValue16);
        }
      }
    }
    function load(l:storage<array<v>>) returns (storage<array<v>>) {
      // "Loading" a storage array field yields its storage reference (the
      // slot). push / pop / length / arr[i] all consume that reference, so a
      // field read like `ArrayPush.push(members, x)` must return the slot,
      // not a copy.
      return l;
    }
}

// Assigning an array literal to a storage array field: `xs = [1,2,3]`.
//
// This is Solidity's memory -> storage array copy. It is a plain function, not
// a CanStore instance, on purpose: instance overlap is decided by the main type
// alone, so a second CanStore instance for storage(array(t)) would clash with
// the deep-copy one above. FieldAccess routes `field = <array literal>` here
// instead of through Assign.assign.
//
// Array.setLength resizes and clears the abandoned tail, so old elements never
// resurrect. The element types differ: `t` is the storage element tag and `v`
// what a value of it looks like in memory (they coincide for word-sized
// elements; for array(string), t = string and v = memory(string)).
function storeArrayLit<t, v>(dst : storage<array<t>>, src : memory<DynArray<v>>) returns (())  where storage<t>: CanStore<v>, v: Typedef<word> {
    let n : word = mload(Typedef.rep(src));
    Array.setLength(dst, uint256(n));
    let base : word = hash1(Typedef.rep(dst));
    let i : word = 0;
    for (; i < n; i += 1) {
        let syntaxValue17: storage<t> = storage(base + i);
        CanStore.store(syntaxValue17, IndexAccess.get(src, uint256(i)));
    }
}

impl CanStore<storage<string>, memory<string>> {
  function store(dst:storage<string>, src:memory<string>) returns (()) {
    let srcPtr : word = Typedef.rep(src);
    let slot = Typedef.rep(dst);
    storeBytesFromMemory(slot, srcPtr);
  }

  function load(src:storage<string>) returns (memory<string>) {
    let srcPtr : word = Typedef.rep(src);
    let dstPtr : word = get_free_memory();
    let endPtr = loadBytesFromStorage(srcPtr, dstPtr);
    set_free_memory(endPtr);
    return memory(dstPtr);
  }
}

// bytes share the same storage layout as string, so the same
// storeBytesFromMemory / loadBytesFromStorage helpers apply.
impl CanStore<storage<bytes>, memory<bytes>> {
  function store(dst:storage<bytes>, src:memory<bytes>) returns (()) {
    let srcPtr : word = Typedef.rep(src);
    let slot = Typedef.rep(dst);
    storeBytesFromMemory(slot, srcPtr);
  }

  function load(src:storage<bytes>) returns (memory<bytes>) {
    let srcPtr : word = Typedef.rep(src);
    let dstPtr : word = get_free_memory();
    let endPtr = loadBytesFromStorage(srcPtr, dstPtr);
    set_free_memory(endPtr);
    return memory(dstPtr);
  }
}

// --- StorageCopy: per-element copy used by whole-array assignment ---

// Word-sized elements are self-contained: the slot is the value.
impl StorageCopy<word> {
  function copySlot(dst:storage<word>, src:storage<word>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}
impl StorageCopy<uint256> {
  function copySlot(dst:storage<uint256>, src:storage<uint256>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}
impl StorageCopy<bytes32> {
  function copySlot(dst:storage<bytes32>, src:storage<bytes32>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}
impl StorageCopy<address> {
  function copySlot(dst:storage<address>, src:storage<address>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// Dynamic elements keep their payload at keccak256(elementSlot), so copying the
// inline slot alone would leave the destination pointing at the *source's* tail.
// Round-tripping through memory copies the payload too.
impl StorageCopy<string> {
  function copySlot(dst:storage<string>, src:storage<string>) returns (()) {
    let syntaxValue18: memory<string> = CanStore.load(src);
    CanStore.store(dst, syntaxValue18);
  }
}
impl StorageCopy<bytes> {
  function copySlot(dst:storage<bytes>, src:storage<bytes>) returns (()) {
    let syntaxValue19: memory<bytes> = CanStore.load(src);
    CanStore.store(dst, syntaxValue19);
  }
}

// Nested arrays recurse into the array CanStore instance above. The recursion is
// on the element type, so it terminates with the type's structure.
impl<t> StorageCopy<array<t>> where t: StorageCopy {
  function copySlot(dst:storage<array<t>>, src:storage<array<t>>) returns (()) {
    CanStore.store(dst, src);
  }
}

// Shamelessly stolen from  function copy_byte_array_to_storage_from_t_bytes_memory_ptr_to_t_bytes_storage
// TODO: consider wrapping behaviour at end of storage
function storeBytesFromMemory(slot: word, src: word) returns (()) {
    let newLen = mload(src);
    // TODO: check old len, cleanup etc
    src += 32; // Move to data.
    match (newLen > 31 ) {
        case true {
            // Long byte array (out-of-place encoding)
            let dstPtr = hash1(slot);
            let loopEnd = newLen & ~0x1f;
            let trailing = loopEnd < newLen;
            loopEnd += src;
            for (; src < loopEnd; src += 32, dstPtr += 1) {
                sstore(dstPtr, mload(src));
            }
            if (trailing) {
                let lastValue = mload(src);
                let lastLen = newLen & 0x1f;
                let mask = ~shr(8 * lastLen, ~0);
                let data_ = lastValue & mask;
                sstore(dstPtr, data_);
            }
            sstore(slot, (newLen * 2) + 1);
        } case false {
            // Short byte array (in-place encoding)
            let value = 0;
            if (newLen != 0) {
                value = mload(src);
            }
            let mask = ~shr(8 * newLen, ~0);
            let data_ = value & mask;
            let used = data_ | (2 * newLen);
            sstore(slot, used);
    } }
}


// shamelessly stolen from abi_encode_t_string_storage_to_t_string_memory_ptr
function loadBytesFromStorage(slot:word, memPtr:word) returns (word) {
    let pos = memPtr;
    let slotValue = sload(slot);
    let length = slotValue / 2;
    let outOfPlaceEncoding = tobool(slotValue & 1);
    if (!outOfPlaceEncoding) {
        length &= 0x7f;
    }
    mstore(pos, length);
    pos += 32;
    match (outOfPlaceEncoding ) {
        case false {
            // Short byte array (in-place encoding)
            mstore(pos, slotValue & ~0xff);
            let empty = iszero(length);
            let notzero = iszero(empty);
            return pos + (notzero * 32);
        } case true {
            // Long byte array (out-of-place encoding)
            let dataPos = hash1(slot);
            let i = 0;
            for (; i < length; i += 32, dataPos += 1) {
                mstore(pos + i, sload(dataPos));
            }
            return pos + i;
    } }
}


// -- Tuple-based indexed access:

trait RValueIdxAccess<col_idx, val> {
  function lookup(ci : col_idx) returns (val);
}

trait LValueIdxAccess<col_idx, ref> {
  function lookup(ci : col_idx) returns (ref);
}

impl<i, a> LValueIdxAccess<(storage<mapping(i => a)>, i), storage<a>> where i: Typedef<word> {
  function lookup(xi : (storage<mapping(i => a)>, i)) returns (storage<a>) {
    match(xi) {
      case (x, i) { return storage(hash2(Typedef.rep(x), Typedef.rep(i)));
    } }
  }
}

impl<i, a> RValueIdxAccess<(storage<mapping(i => a)>, i), a> where storage<a>: CanStore<a>, i: Typedef<word> {
  function lookup(xi : (storage<mapping(i => a)>, i)) returns (a) {
  /*
    match(xi) {
      | (x, i) => return StorageType.load(hash2(Typedef.rep(x), Typedef.rep(i)));
    }
  */
  return readStorage(LValueIdxAccess.lookup(xi));
  }
}

impl<a, i> LValueIdxAccess<(storage<array<a>>, i), storage<a>> where i: Typedef<word> {
  function lookup(xi : (storage<array<a>>, i)) returns (storage<a>) {
    match(xi) {
      case (x, i) {
          let slot : word = Typedef.rep(x);
          let idx : word = Typedef.rep(i);
          // Bounds check: idx must be in [0, length). Length lives at the
          // slot itself; inlined to avoid an Array(t) dispatch here.
          if (idx >= sload(slot)) { out_of_bounds(); }
          return storage(hash1(slot) + idx);
    } }
  }
}

// Reading arr[i] yields whatever the element's storage reference loads, rather
// than the element tag type. For word-sized elements that is the element itself;
// for array(string) it is a memory(string); for a nested array(array(t)) it
// is the inner array's handle, which push/pop/length then consume.
impl<a, v, i> RValueIdxAccess<(storage<array<a>>, i), v> where storage<a>: CanStore<v>, i: Typedef<word> {
  function lookup(xi : (storage<array<a>>, i)) returns (v) {
    return CanStore.load(LValueIdxAccess.lookup(xi));
  }
}

// Indexed read of a lazily-decoded calldata array: `arr[i]` desugars to
// ridx(arr, i), which dispatches here and decodes element i on demand via
// abiArrayGet. There is deliberately no LValueIdxAccess instance — calldata is
// immutable, so `arr[i] = …` is (correctly) rejected at compile time.
impl<t, t_decoded, i> RValueIdxAccess<(calldata<array<t>>, i), t_decoded> where t: ABIAttribs, ABIDecoder<t, CalldataWordReader>: ABIDecode<t_decoded>, i: Typedef<word> {
  function lookup(xi : (calldata<array<t>>, i)) returns (t_decoded) {
    match(xi) {
      case (a, idx) { return abiArrayGet(a, uint256(Typedef.rep(idx)));
    } }
  }
}

// Memory arrays are read-only through `m[i]`: there is no memory cell reference
// type, so they get an RValue instance but no LValue one.
impl<t, i> RValueIdxAccess<(memory<DynArray<t>>, i), t> where t: Typedef<word>, i: Typedef<word> {
  function lookup(xi : (memory<DynArray<t>>, i)) returns (t) {
    match (xi ) {
      case (x, j) { return IndexAccess.get(x, uint256(Typedef.rep(j)));
    } }
  }
}


// Mapping reads go through CanStore, matching the write side (Assign -> CanStore.store).
// This lets a mapping hold any value with a CanStore instance — including ADTs whose
// fields are dynamic (memory(bytes)) — not just the fixed-slot StorageType primitives.
function readStorage<a>(x:storage<a>) returns (a)  where storage<a>: CanStore<a> {
  return CanStore.load(x);
}
/*
forall r a. a:StorageType, r: RValueIdxAccess(a) =>
function rval(x:r) -> a {
  return RValueIdxAccess.lookup(x);
}

forall r a. r: LValueIdxAccess(a) =>
function lval(x:r) -> a {
  return LValueIdxAccess.lookup(x);
}
*/

// lidx/ridx are the generic indexed-access helpers used by the `arr[i]`
// desugaring. They dispatch through LValueIdxAccess / RValueIdxAccess, so any
// collection (mapping, array, ...) that provides those instances supports the
// `arr[i]` syntax.
function lidx<col, idx, ref>(c: col, i: idx) returns (ref)  where (col, idx): LValueIdxAccess<ref> {
    return LValueIdxAccess.lookup((c, i));
}

function ridx<col, idx, val>(c: col, i: idx) returns (val)  where (col, idx): RValueIdxAccess<val> {
    return RValueIdxAccess.lookup((c, i));
}

// --- Memory Encoding ---

trait MemorySize<t> {
    // The size needed for the value.
    function len(v: t) returns (word);
}

// NOTE: this is not implemented for value types.
trait MemoryPointer<t> {
    // In-memory location of the given value.
    function ptr(v: t) returns (word);
}

trait MemoryEncode<t> {
    // Serialize the entire contents at a provided memory area.
    function encodeInto(v: t, target: word) returns (());
}

// TODO: support variadic arguments
// Allocates new memory and concatenates the inputs into it.
function concat<a, b>(x: a, y: b) returns (memory<bytes>)  where a: MemorySize, a: MemoryEncode, b: MemorySize, b: MemoryEncode {
    let x_len = MemorySize.len(x);
    let y_len = MemorySize.len(y);
    let res: word = allocate_memory(32 + x_len + y_len);
    mstore(res, x_len + y_len);
    MemoryEncode.encodeInto(x, res + 32);
    MemoryEncode.encodeInto(y, res + 32 + x_len);
    return memory(res);
}

// This is a specialized 1-input version of concat.
function to_bytes<a>(x: a) returns (memory<bytes>)  where a: MemorySize, a: MemoryEncode {
    let len = MemorySize.len(x);
    let res = allocate_memory(32 + len);
    mstore(res, len);
    MemoryEncode.encodeInto(x, res + 32);
    return memory(res);
}

impl MemorySize<bytes32> {
    function len(v: bytes32) returns (word) {
        return 32;
    }
}

impl MemoryEncode<bytes32> {
    function encodeInto(v: bytes32, target: word) returns (()) {
        mstore(target, Typedef.rep(v));
    }
}

impl MemorySize<memory<bytes>> {
    function len(v: memory<bytes>) returns (word) {
        return mload(Typedef.rep(v));
    }
}

impl MemoryPointer<memory<bytes>> {
    function ptr(v: memory<bytes>) returns (word) {
        return Typedef.rep(v) + 32;
    }
}

impl MemoryEncode<memory<bytes>> {
    function encodeInto(v: memory<bytes>, target: word) returns (()) {
        let v_ = Typedef.rep(v);
        mcopy(target, v_ + 32, mload(v_));
    }
}

// Placeholder for an empty memory area.
// The value is the size of the area in bytes. The area will be zeroed upon serialization.
// NOTE: not implementing Typedef by design.
enum empty { empty(word) }

impl MemorySize<empty> {
    function len(v: empty) returns (word) {
        match (v ) {
            case empty(size) { return size;
        } }
    }
}

impl MemoryEncode<empty> {
    function encodeInto(v: empty, target: word) returns (()) {
        let size;
        match (v ) {
            case empty(size_) { size = size_;
        } }
        zeroize_memory(target, size);
    }
}

// --- Memory Slices ---

// This is a very cheap abstraction over a memory area of [ptr, ptr+len)
// No type information is preserved.
enum memory_ref { memory_ref(word, word) }

impl MemorySize<memory_ref> {
    function len(v: memory_ref) returns (word) {
        match (v ) {
            case memory_ref(ptr, len) { return len;
        } }
    }
}

impl MemoryPointer<memory_ref> {
    function ptr(v: memory_ref) returns (word) {
        match (v ) {
            case memory_ref(ptr, len) { return ptr;
        } }
    }
}

impl MemoryEncode<memory_ref> {
    function encodeInto(v: memory_ref, target: word) returns (()) {
        match (v ) {
            case memory_ref(ptr, len) { mcopy(target, ptr, len);
        } }
    }
}

function slice_<a>(input: a, start: word) returns (memory_ref)  where a: MemorySize, a: MemoryPointer {
    let len = MemorySize.len(input);
    // TODO: should this allow (it does now) a zero-length slice?
    require(len >= start, Error(0xb4120f14)); // OutOfBounds()
    let ptr_ = MemoryPointer.ptr(input);
    return memory_ref(ptr_ + start, len - start);
}

function truncate<a>(input: a, end: word) returns (memory_ref)  where a: MemorySize, a: MemoryPointer {
    let len = MemorySize.len(input);
    // TODO: should this allow (it does now) a zero-length slice?
    require(len >= end, Error(0xb4120f14)); // OutOfBounds()
    return memory_ref(MemoryPointer.ptr(input), end);
}

// --- Hashing ---

// NOTE: keccak256 name conflicts with assembly namespace
function keccak256_<a>(input: a) returns (bytes32)  where a: MemorySize, a: MemoryPointer {
    let len : word = MemorySize.len(input);
    let ptr : word = MemoryPointer.ptr(input);
    return bytes32(keccak256(ptr, len));
}

function sha256<a>(input: a) returns (bytes32)  where a: MemorySize, a: MemoryPointer {
    let len : word = MemorySize.len(input);
    let ptr : word = MemoryPointer.ptr(input);
    // We assume the [0, 32] scratch space is reserved.
    let ret = staticcall(gas(), 2, ptr, len, 0, 32);
    require(ret != 0, Error(0x68c071bb)); // SHA256CallFailed()
    return bytes32(mload(0));
}

function ripemd160<a>(input: a) returns (bytes32)  where a: MemorySize, a: MemoryPointer {
    let len : word = MemorySize.len(input);
    let ptr : word = MemoryPointer.ptr(input);
    // We assume the [0, 32] scratch space is reserved.
    let ret = staticcall(gas(), 3, ptr, len, 0, 32);
    require(ret != 0, Error(0x31a72d92)); // RIPEMD160CallFailed()
    return bytes32(mload(0));
}

// --- Precompiles ---

// Perform an ECDSA signature recovery. It ensures the call has succeeded,
// and that the signature is not malleable (s ≤ secp256k1n/2). Transactions
// were updated to ban this, but the precompile wasn't. If a user relies on that
// feature they can call the precompile via assembly.
// TODO: use uint8
function ecrecover(hash: bytes32, v: uint256, r: bytes32, s: bytes32) returns (address) {
    // MalleableSignatureRejected()
    require(
        Typedef.rep(s) <= 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0,
        Error(0x25260b20)
    );

    let hash_ = Typedef.rep(hash);
    let v_ = Typedef.rep(v);
    let r_ = Typedef.rep(r);
    let s_ = Typedef.rep(s);
    let ptr = get_free_memory();
    // We assume the [0, 32] scratch space is reserved.
    mstore(ptr, hash_);
    mstore(ptr + 32, v_);
    mstore(ptr + 64, r_);
    mstore(ptr + 96, s_);
    // Clear the [0, 32] scratch space that receives the return data. On a
    // failed recovery (e.g. v not in {27, 28}, or the generic could-not-recover
    // case) the precompile still reports success but returns no data, leaving
    // the output area untouched. Without this, a stale non-zero value would
    // slip past the `res != 0` check below and yield a bogus address.
    mstore(0, 0);
    let ret = staticcall(gas(), 1, ptr, 128, 0, 32);
    require(ret != 0, Error(0x578763f7)); // ECRecoverCallFailed()
    let res = mload(0);
    require(res != 0, Error(0x4fbfae63)); // ECRecoverFailed()
    return address(res);
}

// ERC-7201 namespaced storage slot, computed entirely at compile time from a
// string-literal namespace `id`:
//   keccak256(abi.encode(uint256(keccak256(bytes(id))) - 1)) & ~bytes32(uint256(0xff))
function erc7201(comptime id: string) returns (comptime<bytes32>) {
    return bytes32(keccakWordLit(keccakLit(id) - 1) & ~0xff);
}

function raw_call<a>(target: address, value: uint256, payload: a) returns ((bool, memory<bytes>))  where a: MemorySize, a: MemoryPointer {
    let ret = call(
        gas(),
        Typedef.rep(target),
        Typedef.rep(value),
        MemoryPointer.ptr(payload),
        MemorySize.len(payload),
        0,
        0
    );
    let retSize = returndatasize();
    let retData = allocate_memory(32 + retSize);
    mstore(retData, retSize);
    returndatacopy(retData + 32, 0, retSize);
    return (tobool(ret), memory(retData));
}
