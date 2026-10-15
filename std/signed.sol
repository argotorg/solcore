// std.signed: signed integer types int8..int256.
// Each type wraps a two's-complement `word`; narrow widths are kept in range
// (signextend) after every value-producing operation.
import * from std;
import {SigString} from std.dispatch;
import {sdiv, smod, sgt, signextend, mstore, sstore, sload} from std.opcodes;

export { int8, int16, int32, int64, int128, int256 };

// --- int8 (8-bit signed) ---
enum int8 {
  int8(word)
}

impl Typedef<int8, word> {
  function abs(w : word) returns (int8) {
    return int8(w);
  }

  function rep(x : int8) returns (word) {
    match (x) { case int8(w) { return w; } }
  }
}

impl Add<int8> {
  function add(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, Add.add(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Sub<int8> {
  function sub(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, Sub.sub(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mul<int8> {
  function mul(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, Mul.mul(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Div<int8> {
  function div(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, sdiv(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mod<int8> {
  function mod(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, smod(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitAnd<int8> {
  function band(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, BitAnd.band(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitOr<int8> {
  function bor(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, BitOr.bor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitXor<int8> {
  function bxor(x : int8, y : int8) returns (int8) {
    return Typedef.abs(signextend(0, BitXor.bxor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitNot<int8> {
  function bnot(x : int8) returns (int8) {
    return Typedef.abs(signextend(0, BitNot.bnot(Typedef.rep(x))));
  }
}

impl Eq<int8> {
  function eq(x : int8, y : int8) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int8> {
  function gt(x : int8, y : int8) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int8> {
  function minVal() returns (int8) {
    return int8(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80);
  }

  function maxVal() returns (int8) {
    return int8(0x7f);
  }
}

impl Int<int8> {
  function fromInteger(x : integer) returns (int8) {
    return int8(wordFromInteger(x));
  }
}

impl SigString<int8> {
  function sigStr(x : Proxy<int8>) returns (string) {
    return "int8";
  }
}

impl ABIAttribs<int8> {
  function headSize(ty : Proxy<int8>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int8>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int8> {
  function encodeInto(x : int8, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int8, reader>, int8> where reader: WordReader {
  function decode(ptr : ABIDecoder<int8, reader>, currentHeadOffset : word) returns (int8) {
    return Typedef.abs(signextend(0, WordReader.read(WordReader.advance(ptr, currentHeadOffset))));
  }
}

impl StorageSize<int8> {
  function size(x : Proxy<int8>) returns (word) {
    return 1;
  }
}

impl StorageType<int8> {
  function load(ptr : word) returns (int8) {
    return int8(StorageType.load(ptr));
  }

  function store(ptr : word, value : int8) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int8>, int8> {
  function store(l : storage<int8>, r : int8) returns (()) {
    StorageType.store(Typedef.rep(l), r);
  }

  function load(l : storage<int8>) returns (int8) {
    return StorageType.load(Typedef.rep(l));
  }
}

impl StorageCopy<int8> {
  function copySlot(dst : storage<int8>, src : storage<int8>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// --- int16 (16-bit signed) ---
enum int16 {
  int16(word)
}

impl Typedef<int16, word> {
  function abs(w : word) returns (int16) {
    return int16(w);
  }

  function rep(x : int16) returns (word) {
    match (x) {
      case int16(w) {
        return w;
      }
    }
  }
}

impl Add<int16> {
  function add(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, Add.add(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Sub<int16> {
  function sub(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, Sub.sub(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mul<int16> {
  function mul(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, Mul.mul(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Div<int16> {
  function div(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, sdiv(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mod<int16> {
  function mod(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, smod(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitAnd<int16> {
  function band(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, BitAnd.band(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitOr<int16> {
  function bor(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, BitOr.bor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitXor<int16> {
  function bxor(x : int16, y : int16) returns (int16) {
    return Typedef.abs(signextend(1, BitXor.bxor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitNot<int16> {
  function bnot(x : int16) returns (int16) {
    return Typedef.abs(signextend(1, BitNot.bnot(Typedef.rep(x))));
  }
}

impl Eq<int16> {
  function eq(x : int16, y : int16) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int16> {
  function gt(x : int16, y : int16) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int16> {
  function minVal() returns (int16) {
    return int16(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000);
  }

  function maxVal() returns (int16) {
    return int16(0x7fff);
  }
}

impl Int<int16> {
  function fromInteger(x : integer) returns (int16) {
    return int16(wordFromInteger(x));
  }
}

impl SigString<int16> {
  function sigStr(x : Proxy<int16>) returns (string) {
    return "int16";
  }
}

impl ABIAttribs<int16> {
  function headSize(ty : Proxy<int16>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int16>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int16> {
  function encodeInto(x : int16, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int16, reader>, int16> where reader: WordReader {
  function decode(ptr : ABIDecoder<int16, reader>, currentHeadOffset : word) returns (int16) {
    return Typedef.abs(signextend(1, WordReader.read(WordReader.advance(ptr, currentHeadOffset))));
  }
}

impl StorageSize<int16> {
  function size(x : Proxy<int16>) returns (word) {
    return 1;
  }
}

impl StorageType<int16> {
  function load(ptr : word) returns (int16) {
    return int16(StorageType.load(ptr));
  }

  function store(ptr : word, value : int16) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int16>, int16> {
  function store(l : storage<int16>, r : int16) returns (()) {
    StorageType.store(Typedef.rep(l), r);
  }

  function load(l : storage<int16>) returns (int16) {
    return StorageType.load(Typedef.rep(l));
  }
}

impl StorageCopy<int16> {
  function copySlot(dst : storage<int16>, src : storage<int16>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// --- int32 (32-bit signed) ---
enum int32 { int32(word) }
impl Typedef<int32, word> {
  function abs(w : word) returns (int32) {
    return int32(w);
  }

  function rep(x : int32) returns (word) {
    match (x) {
      case int32(w) {
        return w;
      }
    }
  }
}

impl Add<int32> {
  function add(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, Add.add(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Sub<int32> {
  function sub(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, Sub.sub(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mul<int32> {
  function mul(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, Mul.mul(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Div<int32> {
  function div(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, sdiv(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mod<int32> {
  function mod(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, smod(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitAnd<int32> {
  function band(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, BitAnd.band(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitOr<int32> {
  function bor(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, BitOr.bor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitXor<int32> {
  function bxor(x : int32, y : int32) returns (int32) {
    return Typedef.abs(signextend(3, BitXor.bxor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitNot<int32> {
  function bnot(x : int32) returns (int32) {
    return Typedef.abs(signextend(3, BitNot.bnot(Typedef.rep(x))));
  }
}

impl Eq<int32> {
  function eq(x : int32, y : int32) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int32> {
  function gt(x : int32, y : int32) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int32> {
  function minVal() returns (int32) {
    return int32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000);
  }

  function maxVal() returns (int32) {
    return int32(0x7fffffff);
  }
}

impl Int<int32> {
  function fromInteger(x : integer) returns (int32) {
    return int32(wordFromInteger(x));
  }
}

impl SigString<int32> {
  function sigStr(x : Proxy<int32>) returns (string) {
    return "int32";
  }
}

impl ABIAttribs<int32> {
  function headSize(ty : Proxy<int32>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int32>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int32> {
  function encodeInto(x : int32, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int32, reader>, int32> where reader: WordReader {
  function decode(ptr : ABIDecoder<int32, reader>, currentHeadOffset : word) returns (int32) {
    return Typedef.abs(signextend(3, WordReader.read(WordReader.advance(ptr, currentHeadOffset))));
  }
}

impl StorageSize<int32> {
  function size(x : Proxy<int32>) returns (word) {
    return 1;
  }
}

impl StorageType<int32> {
  function load(ptr : word) returns (int32) {
    return int32(StorageType.load(ptr));
  }

  function store(ptr : word, value : int32) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int32>, int32> {
  function store(l : storage<int32>, r : int32) returns (()) {
    StorageType.store(Typedef.rep(l), r);
  }

  function load(l : storage<int32>) returns (int32) {
    return StorageType.load(Typedef.rep(l));
  }
}

impl StorageCopy<int32> {
  function copySlot(dst : storage<int32>, src : storage<int32>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// --- int64 (64-bit signed) ---
enum int64 {
  int64(word)
}

impl Typedef<int64, word> {
  function abs(w : word) returns (int64) {
    return int64(w);
  }

  function rep(x : int64) returns (word) {
    match (x) {
      case int64(w) {
        return w;
      }
    }
  }
}

impl Add<int64> {
  function add(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, Add.add(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Sub<int64> {
  function sub(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, Sub.sub(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mul<int64> {
  function mul(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, Mul.mul(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Div<int64> {
  function div(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, sdiv(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mod<int64> {
  function mod(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, smod(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitAnd<int64> {
  function band(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, BitAnd.band(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitOr<int64> {
  function bor(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, BitOr.bor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitXor<int64> {
  function bxor(x : int64, y : int64) returns (int64) {
    return Typedef.abs(signextend(7, BitXor.bxor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitNot<int64> {
  function bnot(x : int64) returns (int64) {
    return Typedef.abs(signextend(7, BitNot.bnot(Typedef.rep(x))));
  }
}

impl Eq<int64> {
  function eq(x : int64, y : int64) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int64> {
  function gt(x : int64, y : int64) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int64> {
  function minVal() returns (int64) {
    return int64(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000);
  }

  function maxVal() returns (int64) {
    return int64(0x7fffffffffffffff);
  }
}

impl Int<int64> {
  function fromInteger(x : integer) returns (int64) {
    return int64(wordFromInteger(x));
  }
}

impl SigString<int64> {
  function sigStr(x : Proxy<int64>) returns (string) {
    return "int64";
  }
}

impl ABIAttribs<int64> {
  function headSize(ty : Proxy<int64>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int64>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int64> {
  function encodeInto(x : int64, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int64, reader>, int64> where reader: WordReader {
  function decode(ptr : ABIDecoder<int64, reader>, currentHeadOffset : word) returns (int64) {
    return Typedef.abs(signextend(7, WordReader.read(WordReader.advance(ptr, currentHeadOffset))));
  }
}

impl StorageSize<int64> {
  function size(x : Proxy<int64>) returns (word) {
    return 1;
  }
}

impl StorageType<int64> {
  function load(ptr : word) returns (int64) {
    return int64(StorageType.load(ptr));
  }

  function store(ptr : word, value : int64) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int64>, int64> {
  function store(l : storage<int64>, r : int64) returns (()) { StorageType.store(Typedef.rep(l), r); }
  function load(l : storage<int64>) returns (int64) { return StorageType.load(Typedef.rep(l)); }
}

impl StorageCopy<int64> {
  function copySlot(dst : storage<int64>, src : storage<int64>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// --- int128 (128-bit signed) ---
enum int128 {
  int128(word)
}

impl Typedef<int128, word> {
  function abs(w : word) returns (int128) {
    return int128(w);
  }

  function rep(x : int128) returns (word) {
    match (x) {
      case int128(w) {
        return w;
      }
    }
  }
}

impl Add<int128> {
  function add(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, Add.add(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Sub<int128> {
  function sub(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, Sub.sub(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mul<int128> {
  function mul(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, Mul.mul(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Div<int128> {
  function div(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, sdiv(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl Mod<int128> {
  function mod(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, smod(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitAnd<int128> {
  function band(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, BitAnd.band(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitOr<int128> {
  function bor(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, BitOr.bor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitXor<int128> {
  function bxor(x : int128, y : int128) returns (int128) {
    return Typedef.abs(signextend(15, BitXor.bxor(Typedef.rep(x), Typedef.rep(y))));
  }
}

impl BitNot<int128> {
  function bnot(x : int128) returns (int128) {
    return Typedef.abs(signextend(15, BitNot.bnot(Typedef.rep(x))));
  }
}

impl Eq<int128> {
  function eq(x : int128, y : int128) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int128> {
  function gt(x : int128, y : int128) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int128> {
  function minVal() returns (int128) {
    return int128(0xffffffffffffffffffffffffffffffff80000000000000000000000000000000);
  }

  function maxVal() returns (int128) {
    return int128(0x7fffffffffffffffffffffffffffffff);
  }
}

impl Int<int128> {
  function fromInteger(x : integer) returns (int128) {
    return int128(wordFromInteger(x));
  }
}

impl SigString<int128> {
  function sigStr(x : Proxy<int128>) returns (string) {
    return "int128";
  }
}

impl ABIAttribs<int128> {
  function headSize(ty : Proxy<int128>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int128>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int128> {
  function encodeInto(x : int128, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int128, reader>, int128> where reader: WordReader {
  function decode(ptr : ABIDecoder<int128, reader>, currentHeadOffset : word) returns (int128) {
    return Typedef.abs(signextend(15, WordReader.read(WordReader.advance(ptr, currentHeadOffset))));
  }
}

impl StorageSize<int128> {
  function size(x : Proxy<int128>) returns (word) {
    return 1;
  }
}

impl StorageType<int128> {
  function load(ptr : word) returns (int128) {
    return int128(StorageType.load(ptr));
  }

  function store(ptr : word, value : int128) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int128>, int128> {
  function store(l : storage<int128>, r : int128) returns (()) {
    StorageType.store(Typedef.rep(l), r);
  }

  function load(l : storage<int128>) returns (int128) {
    return StorageType.load(Typedef.rep(l));
  }
}

impl StorageCopy<int128> {
  function copySlot(dst : storage<int128>, src : storage<int128>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}

// --- int256 (256-bit signed) ---
enum int256 { int256(word) }
impl Typedef<int256, word> {
  function abs(w : word) returns (int256) {
    return int256(w);
  }

  function rep(x : int256) returns (word) {
    match (x) {
      case int256(w) {
        return w;
      }
    }
  }
}

impl Add<int256> {
  function add(x : int256, y : int256) returns (int256) {
    return Typedef.abs(Add.add(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Sub<int256> {
  function sub(x : int256, y : int256) returns (int256) {
    return Typedef.abs(Sub.sub(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Mul<int256> {
  function mul(x : int256, y : int256) returns (int256) {
    return Typedef.abs(Mul.mul(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Div<int256> {
  function div(x : int256, y : int256) returns (int256) {
    return Typedef.abs(sdiv(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Mod<int256> {
  function mod(x : int256, y : int256) returns (int256) {
    return Typedef.abs(smod(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitAnd<int256> {
  function band(x : int256, y : int256) returns (int256) {
    return Typedef.abs(BitAnd.band(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitOr<int256> {
  function bor(x : int256, y : int256) returns (int256) {
    return Typedef.abs(BitOr.bor(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitXor<int256> {
  function bxor(x : int256, y : int256) returns (int256) {
    return Typedef.abs(BitXor.bxor(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl BitNot<int256> {
  function bnot(x : int256) returns (int256) {
    return Typedef.abs(BitNot.bnot(Typedef.rep(x)));
  }
}

impl Eq<int256> {
  function eq(x : int256, y : int256) returns (bool) {
    return Eq.eq(Typedef.rep(x), Typedef.rep(y));
  }
}

impl Ord<int256> {
  function gt(x : int256, y : int256) returns (bool) {
    return tobool(sgt(Typedef.rep(x), Typedef.rep(y)));
  }
}

impl Bounded<int256> {
  function minVal() returns (int256) {
    return int256(0x8000000000000000000000000000000000000000000000000000000000000000);
  }

  function maxVal() returns (int256) {
    return int256(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);
  }
}

impl Int<int256> {
  function fromInteger(x : integer) returns (int256) {
    return int256(wordFromInteger(x));
  }
}

impl SigString<int256> {
  function sigStr(x : Proxy<int256>) returns (string) {
    return "int256";
  }
}

impl ABIAttribs<int256> {
  function headSize(ty : Proxy<int256>) returns (word) {
    return 32;
  }

  function isStatic(ty : Proxy<int256>) returns (bool) {
    return true;
  }
}

impl ABIEncode<int256> {
  function encodeInto(x : int256, basePtr : word, offset : word, tail : word) returns (word) {
    mstore(basePtr + offset, Typedef.rep(x));
    return tail;
  }
}

impl<reader> ABIDecode<ABIDecoder<int256, reader>, int256> where reader: WordReader {
  function decode(ptr : ABIDecoder<int256, reader>, currentHeadOffset : word) returns (int256) {
    return Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
  }
}

impl StorageSize<int256> {
  function size(x : Proxy<int256>) returns (word) {
    return 1;
  }
}

impl StorageType<int256> {
  function load(ptr : word) returns (int256) {
    return int256(StorageType.load(ptr));
  }

  function store(ptr : word, value : int256) returns (()) {
    StorageType.store(ptr, Typedef.rep(value));
  }
}

impl CanStore<storage<int256>, int256> {
  function store(l : storage<int256>, r : int256) returns (()) {
    StorageType.store(Typedef.rep(l), r);
  }

  function load(l : storage<int256>) returns (int256) {
    return StorageType.load(Typedef.rep(l));
  }
}

impl StorageCopy<int256> {
  function copySlot(dst : storage<int256>, src : storage<int256>) returns (()) {
    sstore(Typedef.rep(dst), sload(Typedef.rep(src)));
  }
}
