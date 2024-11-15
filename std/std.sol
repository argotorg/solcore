/*
- features
    - primitive word eq
    - include stdlib
    - MPTC + optional weak args (MPTC formalization?)
    - surface for loops
    - sugar for IndexAccess read/write (e.g. `x[i]`)
    - explicit variable forall (type constructor in context...)
    - empty types
- syntax
    - brackets around constraint list in forall
    - order of type args
    - braces after match / assembly?
    - braces for blocks in matches
    - trait / impl vs class / instance
    - function -> fn?
*/

// booleans

data bool = true | false;

// TODO: this should short circuit. probably needs some compiler magic to do so.
function and(x: bool, y: bool) -> bool {
    match x, y {
    | true, y => return y;
    | false, _ => return false;
    };
}

// TODO: this should short circuit. probably needs some compiler magic to do so.
function or(x: bool, y: bool) -> bool {
    match x, y {
    | true, _ => return true;
    | false, y => return y;
    };
}

// Pairs

data Pair(a, b) = Pair(a, b);

function fst(p: Pair(a, b)) -> a {
    match p {
    | Pair(a, _) => return a;
    };
}

function snd(p: Pair(a, b)) -> b {
    match p {
    | Pair(_, b) => return b;
    };
}

// Proxy Type for Passing Types as Paramaters

data Proxy(t) = Proxy;

// Type Abstraction

class abs:Typedef(rep) {
    function abs(x:rep) -> abs;
    function rep(x:abs) -> rep;
}

// Arithmetic

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

// Word Arithmetic (unchecked for now...)

instance word:Add {
    function add(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := add(l,r);
        };
        return rw;
    }
}

instance word:Sub {
    function sub(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := sub(l,r);
        };
        return rw;
    }
}

instance word:Mul {
    function mul(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := mul(l,r);
        };
        return rw;
    }
}

instance word:Div {
    function div(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := div(l,r);
        };
        return rw;
    }
}

// uint

data uint = uint(word);
instance uint:Typedef(word) {
    function abs(w: word) -> uint {
        return uint(w);
    }

    function rep(x: uint) -> word {
        match x {
        | uint(w) => return w;
        };
    }
}

// byte

data byte = byte(word);

instance byte:Typedef(word) {
    function abs(w: word) -> byte {
        return byte(w);
    }

    function rep(x: byte) -> word {
        match x {
        | byte(w) => return w;
        };
    }
}

// Generalized References

class ref:Loadable (deref) {
    function load (r : ref) -> deref;
}

class ref:Storable (deref) {
    function store (r : ref, d : deref) -> Unit;
}

class (ref : Loadable(deref), ref : Storable(deref)) => ref:Ref (deref) {}

// Memory References

data memory(t) = memory(word);

instance memory(t) : Typedef(word) {
    function abs(x: word) -> memory(t) {
        return memory(x);
    }

    function rep(x: memory(t)) -> word {
        match x {
        | memory(w) => return w;
        };
    }
}

// Storage References

data storage(t) = storage(word);

instance storage(t) : Typedef(word) {
    function abs(x: word) -> storage(t) {
        return storage(x);
    }

    function rep(x: storage(t)) -> word {
        match x {
        | storage(w) => return w;
       };
    }
}

// Calldata / Returndata (TODO)

data calldata(t) = calldata(word);
data returndata(t) = returndata(word);

// Free Memory Pointer

function get_free_memory() -> word {
    let res : word;
    assembly { res := mload(0x40) };
    return res;
}

function set_free_memory(loc : word) {
    assembly { mstore(0x40, loc) };
}

// Indexable Types

class t:IndexAccess(val) {
    function get(c: t, i: uint) -> val;
    function set(c: t, i: uint, v: val);
}

// DynArray

data DynArray(t) = DynArray(word);

instance DynArray(t):Typedef(word) {
    function abs(w: Word) -> DynArray(t) {
        return DynArray(w);
    }
    function rep(arr: DynArray(t)) -> word {
        match arr {
        | DynArray(w) => return w;
        };
    }
}

instance (t:Typedef(word)) => memory(DynArray(t)):IndexAccess(t) {
    function get(arr : DynArray(t), i : uint) -> t {
        let sz : word;
        let oob : word;
        let iw : word = Typedef.rep(i);
        let loc : word = Typedef.rep(arr);
        let res : word;
        assembly { oob := gt(iw, mload(loc)); };
        match oob {
        | 0 => assembly { res := mload(add(loc, mul(iw, 32))); };
        | _ => assembly { revert(0,0); };
        };
        return Typedef.abs(res);
    }
    function set(arr : DynArray(t), i : uint, val : t) {
        let sz : word;
        let oob : word;
        let iw : word = Typedef.rep(i);
        let loc : word = Typedef.rep(arr);
        let vw : word = Typedef.rep(val);
        assembly { oob := gt(iw, mload(loc)); };
        match oob {
        | 0 => assembly { res := mstore(add(loc, mul(iw, 32)), vw); };
        | _ => assembly { revert(0,0); };
        };
    }
}

// bytes (tightly packed byte arrays)

// TODO: bytes should be indexable (but not ref since it can't live on stack...)
// TODO: we would really want this to be a typedef to void (or a datatype with no constructors)
data bytes = bytes;

// Slices (sized pointers)

data slice(t) = slice(t, word);

// Word Readers: byte indexed structures that can be read in word sized chunks (e.g. calldata / memory)

class ty:WordReader {
    function read(ptr:ty) -> word;
    function advance(ptr:ty, offset:word) -> self;
}

data MemoryWordReader = MemoryWordReader(word);
data CalldataWordReader = CalldataWordReader(word);

instance MemoryWordReader:WordReader {
    function read(reader:MemoryWordReader) -> word {
        let result;
        match reader {
        | MemoryWordReader(ptr) => assembly { result := mload(ptr) };
        };
        return result;
    }
    function advance(reader:MemoryWordReader, offset:word) -> MemoryWordReader {
        match reader {
            | MemoryWordReader(ptr) =>
                assembly { ptr := add(ptr, offset) };
                return MemoryWordReader(ptr);
        };
    }
}
instance CalldataWordReader:WordReader {
    function read(ptr:CalldataWordReader) -> word {
        let result;
        match reader {
        | CalldataWordReader(ptr) => assembly { result := calldataload(ptr) };
        };
        return result;
    }
    function advance(reader:CalldataWordReader, offset:word) -> CalldataWordReader {
        match reader {
        | CalldataWordReader(ptr) =>
            assembly { ptr := add(ptr, offset) };
            return CalldataWordReader(ptr);
        };
    }
}

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


// ABI Metadata

// Tuples in the ABI behave differently to native language tuples
// TODO: I forgot why already @Daniel pls explain to me again (sorry)
data ABITuple(tuple) = ABITuple(tuple);

class self:ABIAttribs {
    function headSize(ty:Proxy(self)) -> word;
    function isStatic(ty:Proxy(self)) -> bool;
}

instance (a:ABIAttribs, b:ABIAttribs) => Pair(a,b):ABIAttribs {
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

instance uint:ABIAttribs {
    function headSize(ty) { return 32; }
    function isStatic(ty) { return true; }
}
instance DynArray(t):ABIAttribs {
    function headSize(ty) { return 32; }
    function isStatic(ty) { return false; }
}

instance (tuple:ABIAttribs) => ABITuple(tuple):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(tuple);
        match ABIAttribs.isStatic(px) {
        | true => return ABIAttribs.headSize(px);
        | false => return 32;
    }
    function isStatic(ty) {
        let px : Proxy(tuple);
        return ABIAttribs.isStatic(px);
    }
}

// TODO: check if these make sense
instance (ty:ABIAttribs) => memory(ty):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(ty);
        return ABIAttribs.headSize(px);
    }
    function isStatic(ty) {
        let px : Proxy(ty);
        return ABIAttribs.isStatic(px);
    }
}
instance (ty:ABIAttribs) => calldata(ty):ABIAttribs {
    function headSize(ty) {
        let px : Proxy(ty);
        return ABIAttribs.headSize(px);
    }
    function isStatic(ty) {
        let px : Proxy(ty);
        return ABIAttribs.isStatic(px);
    }
}

// ABI Encoding

class self:ABIEncode {
    function encodeInto(x:self, basePtr:word, offset:word, tail:word) -> word /* newTail */;
}

instance uint:ABIEncode {
    function encodeInto(x:uint, basePtr:word, offset:word, tail:word) -> word {
        let repx = Typedef.rep(x);
        assembly {
            mstore(add(basePtr, offset), repx)
        };
        return tail;
    }
}

instance (a:ABIAttribs,a:ABIEncode,b:ABIEncode) => Pair(a,b):ABIEncode
{
    function encodeInto(x:Pair(a,b), basePtr:word, offset:word, tail:word) -> word {
        match x {
        | Pair(l,r) =>
            let newTail = ABIEncode.encodeInto(l, basePtr, offset, tail);
            let pa = Proxy(a);
            let a_sz = ABIAttribs.headSize(pa);
            assembly { offset := add(offset, a_sz) };
            return ABIEncode.encodeInto(r, basePtr, offset, newTail);
        };
    }
}


instance (tuple:ABIEncode, tuple:ABIAttribs) => ABITuple(tuple):ABIEncode
{
    function encodeInto(x:ABITuple(tuple), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIEncode.encodeInto(Typedef.rep(x), basePtr, offset, tail);
        | false =>
            // Store tail pointer
            assembly { mstore(head, sub(tail, basePtr)) };
            let prx : Proxy(tuple);
            let headSize = ABIAttribs.headSize(tuple);
            basePtr = tail;
            assembly { tail := add(tail, headSize)  };
            return ABIEncode.encodeInto(Typedef.rep(x), basePtr, 0, tail);
        };
    }
}


// MemoryType (types that can be stored in memory)

// TODO: why do we need the Proxy here?
class self:MemoryType(loadedType) {
    function loadFromMemory(p:Proxy(self), off:word) -> loadedType;
}

// A uint can be loaded from memory and pushed straight onto the stack
instance uint:MemoryType(uint) {
    function loadFromMemory(p:Proxy(uint), off:word) -> uint {
        let v;
        assembly { v := mload(off) };
        return uint256(v);
    }
}

instance (a:MemoryType(a)) => memory(a):Loadable(a) {
    function load(x) {
        let p:Proxy(a);
        match x { | memory(off) => return MemoryType.loadFromMemory(p, off); };
    }
}

// TODO: This seems too restrictive, but allowing the weak param on MemoryType to be free violates the Bound var condition
instance (ty:MemoryType(ty)) => DynArray(ty):MemoryType(slice(memory(ty))) {
    function loadFromMemory(p, off:word) -> slice(memory(ty)) {
        let length;
        assembly {
            length := mload(off)
            off := add(off, 32)
        };
        return slice(Typedef.abs(off), length);
    }
}


// TODO: as above, weak arg on memory type should be free
// TODO: patterson condition
//instance (ty:MemoryType(ty), deref:ABIEncode) => memory(ty):ABIEncode {
    //function encodeInto(x:memory(ty), basePtr:word, offset:word, tail:word) -> word {
        //return ABIEncode.encodeInto(MemoryType.loadFromMemory(Typedef.rep(x)), basePtr, offset, tail);
    //}
//}

// ABI Decoding

class decoder:ABIDecode(decoded) {
    function decode(ptr:decoder, currentHeadOffset:word) -> decoded;
}

// An ABI Decoder for `ty` from `reader`
// This lets us abstract over memory and calldata when decoding
data ABIDecoder(ty, reader) = ABIDecoder(reader);

// If `reader` is a `WordReader` then so is our `ABIDecoder`
instance (reader:WordReader) => ABIDecoder(ty, reader):WordReader {
    function read(decodeer:ABIDecoder(ty, reader)) -> word {
        match decoder {
        | ABIDecoder(ptr) => return WordReader.read(ptr);
        };
    }
    function advance(decoder:ABIDecoder(ty, reader), offset:word) -> ABIDecoder(ty, reader) {
        match decoder {
        | ABIDecoder(ptr) => return ABIDecoder(WordReader.advance(ptr, offset));
        };
    }
}

// ABI Decoding for uint
instance (reader:WordReader) => ABIDecoder(uint, reader):ABIDecode(uint) {
    function decode(ptr:ABIDecoder(uint, reader), currentHeadOffset:word) -> uint {
        return Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
    }
}

// FAIL: Coverage
instance (reader:WordReader, ABIDecoder(a,reader):ABIDecode(a_decoded), ABIDecoder(b,reader):ABIDecode(b_decoded), a:ABIAttribs) => ABIDecoder(Pair(a,b), reader):ABIDecode(Pair(a_decoded,b_decoded))
{
    function decode(ptr:ABIDecoder(Pair(a,b), reader), currentHeadOffset:word) -> Pair(a_decoded, b_decoded) {
        let prx : Proxy(a);
        return (ABIDecoder.decode(ptr, currentHeadOffset), ABIDecoder.decode(ptr, add(currentHeadOffset, ABIAttribs.headSize(prx))));
    }
}

instance (reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs) => ABIDecoder(ABITuple(tuple), reader):ABIDecode(tuple_decoded)
{
    function decode(ptr:ABIDecoder(ABITuple(tuple), reader), currentHeadOffset:word) -> tuple {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecoder.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecoder.decode(WordReader.advance(ptr, tailPtr), 0);
        };
    }
}

instance (reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs) => ABIDecoder(memory(ABITuple(tuple)), reader):ABIDecode(memory(tuple_decoded))
{
    function decode(ptr:ABIDecoder(memory(ABITuple(tuple)), reader), currentHeadOffset:word) -> tuple {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecoder.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecoder.decode(WordReader.advance(ptr, tailPtr), 0);
        };
    }
}

instance (reader:WordReader, ABIDecoder(baseType, reader):ABIDecode(baseType_decoded)) => ABIDecoder(memory(DynArray(baseType)), reader):ABIDecode(memory(DynArray(baseType_decoded)))
{
    function decode(ptr:ABIDecoder(memory(DynArray(baseType)), reader), currentHeadOffset:word) -> memory(DynArray(baseType)) {
        let arrayPtr = WordReader.advance(ptr, currentHeadOffset);
        let length = WordReader.read(arrayPtr);
        let elementPtr:ABIDecoder(baseType, reader) = Typedef.abs(Typedef.rep(WordReader.advance(arrayPtr, 32)));
        arrayPtr = WordReader.advance(32);
        let result:memory(DynArray(baseType_decoded)) = allocateDynamicArray(length);
        let offset = 0;
        let prx : Proxy(baseType);
        let elementHeadSize = ABIAttribs.headSize(base);

        // TODO: surface level loops
        // TODO: sugar for assigning to indexAccess types (result[i])
        //for(let i = 0; i < length; i++) {
            //result[i] = ABIDecode.decode(elementPtr, offset);
            //assembly { offset := add(offset, elementHeadSize) }
        //}

        return result;
    }
}

instance (ABIDecoder(baseType, CalldataWordReader):ABIDecode(baseType_decoded)) => ABIDecoder(calldata(DynArray(baseType)), CalldataWordReader):ABIDecode(calldata(DynArray(baseType_decoded)))
{
    function decode(ptr:ABIDecoder(calldata(DynArray(baseType)), reader), currentHeadOffset:word) -> calldata(DynArray(baseType)) {
        return Typedef.abs(Typedef.rep(Typedef.rep(WordReader.advance(ptr, currentHeadOffset))));
    }
}

// TODO: no instance of ABIDecoder(ty, reader) : ABIDecode (ty)
//forall decodable, reader, ty . (decodable:HasWordReader(reader), ABIDecoder(ty, reader):ABIDecode(ty)) =>
//function abi_decode(decodable:decodable) -> ty
//{
    //return ABIDecode.decode(ABIDecoder(HasWordReader.getWordReader(decodable)):ABIDecoder(ty, decodable), 0);
//}

// Contract Entrypoint

class ty:GenerateDispatch {
    forall f:Invokable(Unit,Unit) . function dispatch_if_selector_match(x: ty) -> f;
}

data Dispatch(name,args,retvals) = Dispatch(name, args, rets);

//// TODO: need to have a way to write predicate in class function or class itself, or have a function type.
//// class self:GenerateDispatch {
////     function  dispatch_if_selector_match(x: self) -> (Unit -> Unit);
//// }
////
//// data Dispatch(name,args,retvals) = DispatchFunction name (args->retvals)
////
//// instance args:ABI => retvals:ABI => name:Selector => dispatchFuncion[name,args,retvals]:GenerateDispatch {
////     function dispatch_if_selector_match(self:dispatchFuncion[name,args,retvals]) -> (() -> ()) {
////         return lambda () {
////             let DispatchFunction(name,f) := self; // or whatever destructuring syntax we want
////             if matches_selector(name) // checks selector in calldata
////             {
////                 let (result_start, result_end) = abi.encode(f(abi.decode(TYPE(retvals)))); // conceptually at least
////                 assembly {
////                     return(result_start, result_end); // as in EVM return, terminating the external call.
////                 }
////             }
////         };
////     }
//// }
////
//// instance a:GenerateDispatch => b:GenerateDispatch => Pair[a,b]:GenerateDispatch {
////     function dispatch_if_selector_match(self:Pair[a,b]) -> (() -> ()) {
////         return lambda () . {
////             dispatch_if_selector_match(self.first);
////             dispatch_if_selector_match(self.second);
////         };
////     }
//// }
////
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
