pragma no-bounded-variable-condition ABIEncode;
pragma no-patterson-condition ABIEncode;
pragma no-coverage-condition ABIDecode, MemoryType;

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

// --- booleans ---

data bool = true | false;

// TODO: this should short circuit. probably needs some compiler magic to do so.
function and(x: bool, y: bool) -> bool {
    match x, y {
    | true, y => return y;
    | false, _ => return false;
    }
}

// TODO: this should short circuit. probably needs some compiler magic to do so.
function or(x: bool, y: bool) -> bool {
    match x, y {
    | true, _ => return true;
    | false, y => return y;
    }
}

// --- Tuple projections ---

function fst(p: (a, b)) -> a {
    match p {
    | (a, _) => return a;
    }
}

function snd(p: (a, b)) -> b {
    match p {
    | (_, b) => return b;
    }
}

// --- Proxy ---

// Proxy is a unit type that can be used to pass Types as paramaters at runtime
data Proxy(t) = Proxy;

// --- Type Abstraction ---

class abs:Typedef(rep) {
    function abs(x:rep) -> abs;
    function rep(x:abs) -> rep;
}

// --- Arithmetic ---

class t:Add {
    function add(l: t, r: t) -> t;
}

class t:Sub {
    function sub(l: t, r: t) -> t;
}

class t:Mul {
    function mul(l: t, r: t) -> t;
}

class t:Div {
    function div(l: t, r: t) -> t;
}

// --- Word Arithmetic ---
// TODO: make these checked

instance word:Add {
    function add(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := add(l,r);
        }
        return rw;
    }
}

instance word:Sub {
    function sub(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := sub(l,r);
        }
        return rw;
    }
}

instance word:Mul {
    function mul(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := mul(l,r);
        }
        return rw;
    }
}

instance word:Div {
    function div(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := div(l,r);
        }
        return rw;
    }
}

// --- Value Types ---

data uint256 = uint256(word);
instance uint256:Typedef(word) {
    function abs(w: word) -> uint256 {
        return uint256(w);
    }

    function rep(x: uint256) -> word {
        match x {
        | uint256(w) => return w;
        }
    }
}

data byte = byte(word);
instance byte:Typedef(word) {
    function abs(w: word) -> byte {
        return byte(w);
    }

    function rep(x: byte) -> word {
        match x {
        | byte(w) => return w;
        }
    }
}

// --- Pointers ---

data memory(t) = memory(word);
instance memory(t) : Typedef(word) {
    function abs(x: word) -> memory(t) {
        return memory(x);
    }

    function rep(x: memory(t)) -> word {
        match x {
        | memory(w) => return w;
        }
    }
}

data storage(t) = storage(word);
instance storage(t) : Typedef(word) {
    function abs(x: word) -> storage(t) {
        return storage(x);
    }

    function rep(x: storage(t)) -> word {
        match x {
        | storage(w) => return w;
       }
    }
}

data calldata(t) = calldata(word);
instance calldata(t) : Typedef(word) {
    function abs(x: word) -> calldata(t) {
        return calldata(x);
    }

    function rep(x: calldata(t)) -> word {
        match x {
        | calldata(w) => return w;
       }
    }
}

data returndata(t) = returndata(word);
instance returndata(t) : Typedef(word) {
    function abs(x: word) -> calldata(t) {
        return returndata(x);
    }

    function rep(x: returndata(t)) -> word {
        match x {
        | returndata(w) => return w;
       }
    }
}

// --- Free Memory Pointer ---

// Memory in solidity is bump allocated in a single arena
// The word stored in memory at index 0x40 is used to store the start of the currently unused memory region

// returns the value stored in memory(0x40)
function get_free_memory() -> word {
    let res : word;
    assembly { res := mload(0x40) }
    return res;
}

// set the value stored in memory(0x40)
function set_free_memory(loc : word) {
    assembly { mstore(0x40, loc) }
}

// --- Indexable Types ---

// types that can be written to and read from at a uint256 index
// TODO: this needs to be split into LValue / RValue variants for `=` desugaring
class t:IndexAccess(val) {
    function get(c: t, i: uint256) -> val;
    function set(c: t, i: uint256, v: val);
}

// --- DynArray ---

// Word arrays with a size known only at runtime
// types with a size smaller than `word` will not be packed, so a `DynArray(byte)` will waste a lot of space
// TODO: storage representation
data DynArray(t);

forall t . t:Typedef(word) => instance memory(DynArray(t)):IndexAccess(t) {
    function get(ptr, i) {
        let sz : word;
        let oob : word;
        let iw : word = Typedef.rep(i);
        let loc : word = Typedef.rep(ptr);
        let res : word;
        assembly { oob := gt(iw, mload(loc)); }
        match oob {
        | 0 => assembly { res := mload(add(loc, mul(iw, 32))); }
        | _ => assembly { revert(0,0); }
        }
        return Typedef.abs(res);
    }
    function set(arr, i, val) {
        let sz : word;
        let oob : word;
        let iw : word = Typedef.rep(i);
        let loc : word = Typedef.rep(arr);
        let vw : word = Typedef.rep(val);
        assembly { oob := gt(iw, mload(loc)); }
        match oob {
        | 0 => assembly { res := mstore(add(loc, mul(iw, 32)), vw); }
        | _ => assembly { revert(0,0); }
        }
    }
}

function allocateDynamicArray(prx : Proxy(t), length : word) -> memory(DynArray(t)) {
    // size of allocation in bytes
    let sz : word = Mul.mul(Add.add(length, 1), 32);

    // get start of array & increment free by sz
    let free : word = get_free_memory();
    set_free_memory(Add.add(free, sz));

    // write array length and return
    assembly { mstore(free, length) }
    let res : memory(DynArray(t)) = Typedef.abs(free);
    return res;
}

// --- bytes ---

// tightly packed byte arrays
// bytes does not have a runtime representation since it can only ever exist in
// memory / calldata / storage and serves only as a type tag for pointer types
// TODO: IndexAccess for memory(bytes)
// TODO: IndexAccess for calldata(bytes)
// TODO: IndexAccess for storage(bytes)
data bytes;

// --- slices (sized pointers) ---

// A slice is a wrapper around an existing pointer type that extends the
// underlying type with information about the size of the data pointed to by `t`
data slice(ptr) = slice(ptr, word);

// --- Word Readers ---

// A WordReader is an abstraction over byte indexed structure that can be read in word sized chunks (e.g. calldata / memory)
// These let us use the same abi decoding routines for calldata / memory
class ty:WordReader {
    // returns the word currently pointed to by the WordReader
    function read(reader:ty) -> word;
    // returns a new WordReader that points to a location `offset` bytes further into the array
    function advance(reader:ty, offset:word) -> ty;
}

// WordReader for memory
data MemoryWordReader = MemoryWordReader(word);
instance MemoryWordReader:WordReader {
    function read(reader:MemoryWordReader) -> word {
        let result : word;
        match reader {
        | MemoryWordReader(ptr) => assembly { result := mload(ptr) }
        }
        return result;
    }
    function advance(reader:MemoryWordReader, offset:word) -> MemoryWordReader {
        match reader {
        | MemoryWordReader(ptr) => return MemoryWordReader(Add.add(ptr, offset));
        }
    }
}

// WordReader for calldata
data CalldataWordReader = CalldataWordReader(word);
instance CalldataWordReader:WordReader {
    function read(reader:CalldataWordReader) -> word {
        let result : word;
        match reader {
        | CalldataWordReader(ptr) => assembly { result := calldataload(ptr) }
        }
        return result;
    }
    function advance(reader:CalldataWordReader, offset:word) -> CalldataWordReader {
        match reader {
        | CalldataWordReader(ptr) => return CalldataWordReader(Add.add(ptr, offset));
        }
    }
}

// --- HasWordReader ---

// The HasWordReader class defines the types for which a WordReader can be produced
// We define instances for memory(bytes) and calldata(bytes)
class self:HasWordReader(reader) {
    function getWordReader(x:self) -> reader;
}

instance memory(bytes):HasWordReader(MemoryWordReader) {
    function getWordReader(x:memory(bytes)) -> MemoryWordReader {
        return MemoryWordReader(Typedef.rep(x));
    }
}

instance calldata(bytes):HasWordReader(CalldataWordReader) {
    function getWordReader(x:calldata(bytes)) -> CalldataWordReader {
        return CalldataWordReader(Typedef.rep(x));
    }
}

// --- MemoryType ---

// A MemoryType instance abstracts over type specific logic related to memory
// layout, allowing us to write code that is generic over which type is held in memory
class self:MemoryType(loadedType) {
    // Proxy needed becaused class methods must mention strong type params
    // loads an instance of `loadedType` from an instance of `self` located at `loc` in memory
    function loadFromMemory(p:Proxy(self), loc:word) -> loadedType;
}

// A uint256 can be loaded from memory and pushed straight onto the stack
instance uint256:MemoryType(uint256) {
    function loadFromMemory(p:Proxy(uint256), loc:word) -> uint256 {
        let v;
        assembly { v := mload(loc) }
        return uint256(v);
    }
}

// We load a DynArray into a sized pointer to the first element
forall ty ret . ty:MemoryType(ret) => instance DynArray(ty):MemoryType(slice(memory(ret))) {
    function loadFromMemory(p, loc:word) -> slice(memory(ret)) {
        let length;
        assembly {
            length := mload(loc)
            off := add(loc, 32)
        }
        return slice(Typedef.abs(loc) : memory(ret), length);
    }
}


// FAIL: patterson
// FAIL: bound variable
// if we ty is a MemoryType that returns deref and deref is ABIEncode, then we can encode a memory(ty)
// by loading it and then running the ABI encoding for the loaded value
forall ty deref . ty:MemoryType(deref), deref:ABIEncode => instance memory(ty):ABIEncode {
    function encodeInto(x:memory(ty), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(deref);
        return ABIEncode.encodeInto(MemoryType.loadFromMemory(prx, Typedef.rep(x)), basePtr, offset, tail);
    }
}

// --- ABI Tuples ---

// Tuples in Solidity are always desugared to nested pairs (to allow for
// inductive typeclass instance constructions) .
// This is an issue for the ABI routines since the ABI spec differentiates
// between `(1,1,1)` and `(1,(1,1))`, but the language treats both identically.
// The ABITuple type lets us reiintroduce this distinction:
// `ABITuple((1,(1,1))` should be treated as `(1,1,1)`  for the purposes of ABI
// encoding / decoding.
data ABITuple(tuple) = ABITuple(tuple);

instance ABITuple(t):Typedef(t) {
    function abs(t: t) -> ABITuple(t) {
        return ABITuple(t);
    }

    function rep(x: ABITuple(t)) -> t {
        match x {
        | ABITuple(v) => return v;
        }
    }
}

// --- ABI Metadata ---

// Statically knowable ABI related metadata about `self`
class self:ABIAttribs {
    // how many bytes should be used for the head portion of the abi encoding of `self`
    function headSize(ty:Proxy(self)) -> word;
    // whether or not `self` is a fully static type
    function isStatic(ty:Proxy(self)) -> bool;
}

instance uint256:ABIAttribs {
    function headSize(ty) { return 32; }
    function isStatic(ty) { return true; }
}
instance DynArray(t):ABIAttribs {
    function headSize(ty) { return 32; }
    function isStatic(ty) { return false; }
}

// computes the attribs for a pair of two types that implement attribs
forall a b . a:ABIAttribs, b:ABIAttribs => instance (a,b):ABIAttribs {
    function headSize(ty) {
        let pa : Proxy(a);
        let pb : Proxy(b);
        let sza = ABIAttribs.headSize(pa);
        let szb = ABIAttribs.headSize(pb);
        return Add.add(sza, szb);
    }
    function isStatic(ty) {
        let pa : Proxy(a);
        let pb : Proxy(b);
        return and(ABIAttribs.isStatic(pa), ABIAttribs.isStatic(pb));
    }
}

// if an abi tuple contains dynamic elems we store it in the tail, otherwise we
// treat it the same as a series of nested pairs
forall tuple . tuple:ABIAttribs => instance ABITuple(tuple):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(tuple);
        match ABIAttribs.isStatic(px) {
        | true => return ABIAttribs.headSize(px);
        | false => return 32;
        }
    }
    function isStatic(ty) {
        let px : Proxy(tuple);
        return ABIAttribs.isStatic(px);
    }
}

// for pointer types we fetch the attribs of the pointed to type, not the pointer itself
forall ty . ty:ABIAttribs => instance memory(ty):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(ty);
        return ABIAttribs.headSize(px);
    }
    function isStatic(ty) {
        let px : Proxy(ty);
        return ABIAttribs.isStatic(px);
    }
}
forall ty . ty:ABIAttribs => instance calldata(ty):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(ty);
        return ABIAttribs.headSize(px);
    }
    function isStatic(ty) {
        let px : Proxy(ty);
        return ABIAttribs.isStatic(px);
    }
}

// --- ABI Encoding ---
// TODO: make these generic over the location being written to (i.e. memory or returndata)

// top level encoding function.
// abi encodes an instance of `ty` and returns a pointer to the result
forall ty . ty:ABIAttribs, ty:ABIEncode => function abi_encode(val : ty) -> memory(bytes) {
    let free = get_free_memory();
    let tail = ABIEncode.encodeInto(val, free, 0, Add.add(free, ABIAttribs.headSize(Proxy : Proxy(ty))));
    set_free_memory(tail);
    return memory(free);
}

// types that can be abi encoded
class self:ABIEncode {
    // abi encodes an instance of self into a memory region starting at basePtr
    // offset gives the offset in memory from basePtr to the first empty byte of the head
    // tail gives the index in memory of the first empty byte of the tail
    function encodeInto(x:self, basePtr:word, offset:word, tail:word) -> word /* newTail */;
}

instance uint256:ABIEncode {
    // a unit256 is written directly into the head
    function encodeInto(x:uint256, basePtr:word, offset:word, tail:word) -> word {
        let repx : word = Typedef.rep(x);
        assembly { mstore(add(basePtr, offset), repx) }
        return tail;
    }
}

// abi encoding for a pair of two encodable types
forall a b . a:ABIAttribs, a:ABIEncode, b:ABIEncode => instance (a,b):ABIEncode {
    function encodeInto(x: (a,b), basePtr: word, offset: word, tail: word) -> word {
        match x {
        | (l,r) =>
            let newTail = ABIEncode.encodeInto(l, basePtr, offset, tail);
            let pa : Proxy(a);
            let a_sz = ABIAttribs.headSize(pa);
            return ABIEncode.encodeInto(r, basePtr, Add.add(offset, a_sz), newTail);
        }
    }
}


// abi encoding for an ABITuple of encodable types
// TODO: is this correct?
forall tuple . tuple:ABIEncode, tuple:ABIAttribs => instance ABITuple(tuple):ABIEncode {
    function encodeInto(x:ABITuple(tuple), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        // if the tuple contains only static elements then we encode it in the head
        | true => return ABIEncode.encodeInto(Typedef.rep(x), basePtr, offset, tail);
        // if the tuple contains dynamically sized elements then we store a
        // pointer in the head, and encode the tuple into the tail
        | false =>
            // store the length of the head in basePtr
            assembly { mstore(basePtr, sub(tail, basePtr)) }

            // encode the underlying tuple into the tail
            let headSize = ABIAttribs.headSize(Proxy : Proxy(tuple));
            basePtr = tail;
            assembly { tail := add(tail, headSize) }
            return ABIEncode.encodeInto(Typedef.rep(x), basePtr, 0, tail);
        }
    }
}

// --- ABI Decoding ---

// Top level decoding function.
// abi decodes an instance of `decodable` into a `ty`
forall decodable reader ty . decodable:HasWordReader(reader), ABIDecoder(ty, reader):ABIDecode(decoded) =>
function abi_decode(decodable:decodable) -> decoded {
    let decoder : ABIDecoder(ty, reader) = ABIDecoder(HasWordReader.getWordReader(decodable));
    return ABIDecode.decode(decoder, 0);
}


class decoder:ABIDecode(decoded) {
    function decode(ptr:decoder, currentHeadOffset:word) -> decoded;
}

// An ABI Decoder for `ty` from `reader`
// This lets us abstract over memory and calldata when decoding
data ABIDecoder(ty, reader) = ABIDecoder(reader);

// If `reader` is a `WordReader` then so is our `ABIDecoder`
forall ty reader . reader:WordReader => instance ABIDecoder(ty, reader):WordReader {
    function read(decoder:ABIDecoder(ty, reader)) -> word {
        match decoder {
        | ABIDecoder(ptr) => return WordReader.read(ptr);
        }
    }
    function advance(decoder:ABIDecoder(ty, reader), offset:word) -> ABIDecoder(ty, reader) {
        match decoder {
        | ABIDecoder(ptr) => return ABIDecoder(WordReader.advance(ptr, offset));
        }
    }
}

// ABI Decoding for uint
forall reader . reader:WordReader => instance ABIDecoder(uint256, reader):ABIDecode(uint256) {
    function decode(ptr:ABIDecoder(uint256, reader), currentHeadOffset:word) -> uint256 {
        return Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset))) : uint256;
    }
}

// ABI decoding for a pait of decodable values
// FAIL: Coverage
forall a b a_decoded b_decoded reader . reader:WordReader, ABIDecoder(b,reader):ABIDecode(b_decoded), ABIDecoder(a,reader):ABIDecode(a_decoded), a:ABIAttribs => instance ABIDecoder((a,b), reader):ABIDecode((a_decoded,b_decoded))
{
    function decode(ptr:ABIDecoder((a,b), reader), currentHeadOffset:word) -> (a_decoded, b_decoded) {
        match ptr {
        | ABIDecoder(rdr) =>
            let prx : Proxy(a);
            let decoder_a : ABIDecoder(a, reader) = ABIDecoder(rdr);
            let decoder_b : ABIDecoder(b, reader) = ABIDecoder(rdr);
            let a_val : a_decoded = ABIDecode.decode(decoder_a, currentHeadOffset);
            let b_val : b_decoded = ABIDecode.decode(decoder_b, Add.add(currentHeadOffset, ABIAttribs.headSize(prx)));
            return (a_val, b_val);
        }
    }
}

forall reader tuple tuple_decoded . reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs =>
    instance ABIDecoder(ABITuple(tuple), reader):ABIDecode(tuple_decoded)
{
    function decode(ptr:ABIDecoder(ABITuple(tuple), reader), currentHeadOffset:word) -> tuple_decoded {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
       }
    }
}

forall reader tuple tuple_decoded . reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs =>
    instance ABIDecoder(memory(ABITuple(tuple)), reader):ABIDecode(memory(tuple_decoded))
{
    function decode(ptr:ABIDecoder(memory(ABITuple(tuple)), reader), currentHeadOffset:word) -> memory(tuple_decoded) {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
        }
    }
}

forall reader baseType reader baseType_decoded .baseType : ABIAttribs, reader:WordReader, ABIDecoder(baseType, reader):ABIDecode(baseType_decoded) =>
    instance ABIDecoder(memory(DynArray(baseType)), reader):ABIDecode(memory(DynArray(baseType_decoded)))
{
    function decode(ptr:ABIDecoder(memory(DynArray(baseType)), reader), currentHeadOffset:word) -> memory(DynArray(baseType)) {
        let arrayPtr = WordReader.advance(ptr, currentHeadOffset);
        let length = WordReader.read(arrayPtr);
        // this trigger a missing typedef constraint
        // let elementPtr:ABIDecoder(baseType, reader) = Typedef.abs(WordReader.advance(arrayPtr, 32));
        arrayPtr = WordReader.advance(arrayPtr, 32);
        let prx : Proxy(baseType_decoded);
        let result : memory(DynArray(baseType_decoded)) = allocateDynamicArray(prx, length);
        let offset = 0;
        let prx : Proxy(baseType);
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

// missing constraints.
// forall baseType baseType_decoded . ABIDecoder(baseType, CalldataWordReader):ABIDecode(baseType_decoded),
//     baseType : WordReader =>
//     instance ABIDecoder(calldata(DynArray(baseType)), CalldataWordReader):ABIDecode(calldata(DynArray(baseType_decoded)))
// {
//     function decode(ptr:ABIDecoder(calldata(DynArray(baseType)), reader), currentHeadOffset:word) -> calldata(DynArray(baseType)) {
//         return Typedef.abs(Typedef.rep(WordReader.advance(ptr, currentHeadOffset)));
//     }
// }
//
// --- Contract Entrypoint ---

class nm:Selector {}

class ty:GenerateDispatch {
    forall f . f:Invokable((),()) => function dispatch_if_selector_match(x: ty) -> f;
}

data Dispatch(name, args, retvals, f) = Dispatch(name, args, rets, f);

forall name . name:Selector => function selector_matches(nm : name) -> bool {
    return true;
}

// ERROR: Types: (t_closure4(o55, p55, q55, r55), ()) and t_closure4(w55, a, r, f) do not unify
//forall nm args rets f g . nm:Selector, args:ABIDecode, rets:ABIEncode, f:Invokable(args, rets) => instance Dispatch(nm,args,rets,f):GenerateDispatch {
    //function dispatch_if_selector_match(d:Dispatch(n,a,r,f)) -> g {
        //return lam() {
            //match d {
            //| Dispatch(name, args, rets, fn) => match selector_matches(name) {
                //| false => return ();
                //| true => return ();
            //}}
        //};
    //}
//}

//// /// Translation of the above contract
//// struct StorageContext {
////     x:uint;
////     y:bool;
//// }
////
//// function C_f(ctxt:StorageContext) public {
////     ctxt.x = 42;
//// }
////
////
//// function entry_C() {
////     GenerateDispatch.dispatch_if_selector_match(DispatchFunction("f()", C_f)); // could also be (nested) pairs of dispatch functions, if the contract had more functions
////     revert("unknown selector");
//// }
////
//// // init code for contract creation
//// function init_C() {
////     // constructor code
////     let code_start := allocate_unbounded() // fetch some free memory
////     let code_length := __builtin_fetch_code(entry_C, code_start) // sounds weirder than it is - this will just add the code for entry_C to a Yul subobject and use some Yul builtins for fetching the code to be deployed
////     assembly {
////         return(code_start, code_length)
////     }
//// }
////
////
