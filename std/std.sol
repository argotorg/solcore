pragma no-bounded-variable-condition ABIEncode;
pragma no-patterson-condition ABIEncode;
pragma no-coverage-condition ABIDecode, MemoryType;
/*
- features
    - primitive word eq
    - include stdlib
    - MPTC + optional weak args (MPTC formalization?)
    - surface for loops
    - empty types
    - better inference for Typedef.rep() calls (have to annotate atm?)
    - inline Proxy defs
    - boolean short circuiting
- sugar
    - Proxy (e.g. `@t ==> Proxy : Proxy t`
    - Pair (e.g. `(a,b) ==> Pair(a,b)`
    - IndexAccess reads (e.g. `x[i] ==> IndexAccess.get(x, i)`)
    - auto typedef instances
- syntax
    - brackets around constraint list in forall
    - order of type args
    - braces after match / assembly?
    - braces for blocks in matches
    - trait / impl vs class / instance
    - function -> fn?
    - assembly vs high level return?
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

// uint256

data uint256 = uint256(word);
instance uint256:Typedef(word) {
    function abs(w: word) -> uint256 {
        return uint256(w);
    }

    function rep(x: uint256) -> word {
        match x {
        | uint256(w) => return w;
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

// Calldata

data calldata(t) = calldata(word);

instance calldata(t) : Typedef(word) {
    function abs(x: word) -> calldata(t) {
        return calldata(x);
    }

    function rep(x: calldata(t)) -> word {
        match x {
        | calldata(w) => return w;
       };
    }
}

// Returndata

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
    function get(c: t, i: uint256) -> val;
    function set(c: t, i: uint256, v: val);
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
    function get(arr : DynArray(t), i : uint256) -> t {
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
    function set(arr : DynArray(t), i : uint256, val : t) {
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
        let result : word;
        match reader {
            | MemoryWordReader(ptr) => assembly { result := mload(ptr) };
        };
        return result;
    }
    function advance(reader:MemoryWordReader, offset:word) -> MemoryWordReader {
        match reader {
            | MemoryWordReader(ptr) => return MemoryWordReader(Add.add(ptr, offset));
        };
    }
}
instance CalldataWordReader:WordReader {
    function read(reader:CalldataWordReader) -> word {
        let result : word;
        match reader {
            | CalldataWordReader(ptr) => assembly { result := calldataload(ptr) };
        };
        return result;
    }
    function advance(reader:CalldataWordReader, offset:word) -> CalldataWordReader {
        match reader {
            | CalldataWordReader(ptr) => return CalldataWordReader(Add.add(ptr, offset));
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

instance ABITuple(t):Typedef(t) {
    function abs(t: t) -> ABITuple(t) {
        return ABITuple(t);
    }

    function rep(x: ABITuple(t)) -> t {
        match x {
        | ABITuple(v) => return v;
        };
    }
}


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

instance uint256:ABIAttribs {
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
        };
    }
    function isStatic(ty) {
        let px : Proxy(tuple);
        return ABIAttribs.isStatic(px);
    }
}

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

instance uint256:ABIEncode {
    function encodeInto(x:uint256, basePtr:word, offset:word, tail:word) -> word {
        let repx : word = Typedef.rep(x);
        assembly { mstore(add(basePtr, offset), repx) };
        return tail;
    }
}

instance (a:ABIAttribs,a:ABIEncode,b:ABIEncode) => Pair(a,b):ABIEncode {
    function encodeInto(x: Pair(a,b), basePtr: word, offset: word, tail: word) -> word {
        match x {
        | Pair(l,r) =>
            let newTail = ABIEncode.encodeInto(l, basePtr, offset, tail);
            let pa : Proxy(a);
            let a_sz = ABIAttribs.headSize(pa);
            return ABIEncode.encodeInto(r, basePtr, Add.add(offset, a_sz), newTail);
        };
    }
}


instance (tuple:ABIEncode, tuple:ABIAttribs) => ABITuple(tuple):ABIEncode {
    function encodeInto(x:ABITuple(tuple), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIEncode.encodeInto(Typedef.rep(x), basePtr, offset, tail);
        | false =>
            // Store tail pointer
            assembly { mstore(basePtr, sub(tail, basePtr)) };
            let prx : Proxy(tuple);
            let headSize = ABIAttribs.headSize(prx);
            basePtr = tail;
            assembly { tail := add(tail, headSize)  };
            return ABIEncode.encodeInto(Typedef.rep(x), basePtr, 0, tail);
        };
    }
}


// MemoryType (types that can be stored in memory)

class self:MemoryType(loadedType) {
    // Proxy needed becaused class methods must mention strong type params
    function loadFromMemory(p:Proxy(self), off:word) -> loadedType;
}

// A uint256 can be loaded from memory and pushed straight onto the stack
instance uint256:MemoryType(uint256) {
    function loadFromMemory(p:Proxy(uint256), off:word) -> uint256 {
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

// FAIL: bound variable
instance (ty:MemoryType(ret)) => DynArray(ty):MemoryType(slice(memory(ret))) {
    function loadFromMemory(p, off:word) -> slice(memory(ret)) {
        let length;
        assembly {
            length := mload(off)
            off := add(off, 32)
        };
        return slice(Typedef.abs(off), length);
    }
}


// FAIL: patterson
// FAIL: bound variable
instance (ty:MemoryType(deref), deref:ABIEncode) => memory(ty):ABIEncode {
    function encodeInto(x:memory(ty), basePtr:word, offset:word, tail:word) -> word {
        let prx : Proxy(deref);
        return ABIEncode.encodeInto(MemoryType.loadFromMemory(prx, Typedef.rep(x)), basePtr, offset, tail);
    }
}

// ABI Decoding

class decoder:ABIDecode(decoded) {
    function decode(ptr:decoder, currentHeadOffset:word) -> decoded;
}

// An ABI Decoder for `ty` from `reader`
// This lets us abstract over memory and calldata when decoding
data ABIDecoder(ty, reader) = ABIDecoder(reader);

// If `reader` is a `WordReader` then so is our `ABIDecoder`
instance (reader:WordReader) => ABIDecoder(ty, reader):WordReader {
    function read(decoder:ABIDecoder(ty, reader)) -> word {
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
instance (reader:WordReader) => ABIDecoder(uint256, reader):ABIDecode(uint256) {
    function decode(ptr:ABIDecoder(uint256, reader), currentHeadOffset:word) -> uint256 {
        return Typedef.abs(WordReader.read(WordReader.advance(ptr, currentHeadOffset)));
    }
}

// FAIL: Coverage
// TODO: no instance of ABIDecoder(b, reader) : ABIDecode (b_decoded)
// TODO: file bug
//instance (reader:WordReader, ABIDecoder(b,reader):ABIDecode(b_decoded), ABIDecoder(a,reader):ABIDecode(a_decoded), a:ABIAttribs) => ABIDecoder(Pair(a,b), reader):ABIDecode(Pair(a_decoded,b_decoded))
//{
    //function decode(ptr:ABIDecoder(Pair(a,b), reader), currentHeadOffset:word) -> Pair(a_decoded, b_decoded) {
        //let prx : Proxy(a);
        //return Pair(ABIDecode.decode(ptr, currentHeadOffset), ABIDecode.decode(ptr, Add.add(currentHeadOffset, ABIAttribs.headSize(prx))));
    //}
//}

instance (reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs) => ABIDecoder(ABITuple(tuple), reader):ABIDecode(tuple_decoded)
{
    function decode(ptr:ABIDecoder(ABITuple(tuple), reader), currentHeadOffset:word) -> tuple {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
        };
    }
}

instance (reader:WordReader, tuple:ABIDecode(tuple_decoded), tuple:ABIAttribs) => ABIDecoder(memory(ABITuple(tuple)), reader):ABIDecode(memory(tuple_decoded))
{
    function decode(ptr:ABIDecoder(memory(ABITuple(tuple)), reader), currentHeadOffset:word) -> tuple {
        let prx : Proxy(tuple);
        match ABIAttribs.isStatic(prx) {
        | true => return ABIDecode.decode(WordReader.advance(ptr, currentHeadOffset), 0);
        | false =>
            let tailPtr = WordReader.read(ptr);
            return ABIDecode.decode(WordReader.advance(ptr, tailPtr), 0);
        };
    }
}

function allocateDynamicArray(prx : Proxy(t), length : word) -> memory(DynArray(t)) {
    // size of allocation in bytes
    let sz : word = Mul.mul(Add.add(length, 1), 32);

    // get start of array & increment free by sz
    let free : word = get_free_memory();
    set_free_memory(Add.add(free, sz));

    // write array length and return
    assembly { mstore(free, length) };
    let res : memory(DynArray(t)) = Typedef.abs(free);
    return res;
}

// no instance of ABIDecoder(baseType, reader) : ABIDecode (baseType_decoded)
//instance (reader:WordReader, ABIDecoder(baseType, reader):ABIDecode(baseType_decoded)) => ABIDecoder(memory(DynArray(baseType)), reader):ABIDecode(memory(DynArray(baseType_decoded)))
//{
    //function decode(ptr:ABIDecoder(memory(DynArray(baseType)), reader), currentHeadOffset:word) -> memory(DynArray(baseType)) {
        //let arrayPtr = WordReader.advance(ptr, currentHeadOffset);
        //let length = WordReader.read(arrayPtr);
        //let elementPtr:ABIDecoder(baseType, reader) = Typedef.abs(WordReader.advance(arrayPtr, 32));
        //arrayPtr = WordReader.advance(arrayPtr, 32);
        //let prx : Proxy(baseType_decoded);
        //let result : memory(DynArray(baseType_decoded)) = allocateDynamicArray(prx, length);
        //let offset = 0;
        //let prx : Proxy(baseType);
        //let elementHeadSize = ABIAttribs.headSize(prx);

        //// TODO: surface level loops
        //// TODO: sugar for assigning to indexAccess types (result[i])
        ////for(let i = 0; i < length; i++) {
            ////result[i] = ABIDecode.decode(elementPtr, offset);
            ////assembly { offset := add(offset, elementHeadSize) }
        ////}

        //return result;
    //}
//}

// no instance of ABIDecoder(baseType, CalldataWordReader) : ABIDecode (baseType_decoded)
//instance (ABIDecoder(baseType, CalldataWordReader):ABIDecode(baseType_decoded)) => ABIDecoder(calldata(DynArray(baseType)), CalldataWordReader):ABIDecode(calldata(DynArray(baseType_decoded)))
//{
    //function decode(ptr:ABIDecoder(calldata(DynArray(baseType)), reader), currentHeadOffset:word) -> calldata(DynArray(baseType)) {
        //return Typedef.abs(Typedef.rep(Typedef.rep(WordReader.advance(ptr, currentHeadOffset))));
    //}
//}

// TODO: no instance of ABIDecoder(ty, reader) : ABIDecode (ty)
// TODO: is this instance too strict
//forall decodable, reader, ty . (decodable:HasWordReader(reader), ABIDecoder(ty, reader):ABIDecode(ty)) =>
//function abi_decode(decodable:decodable) -> ty
//{
    //let decoder : ABIDecoder(ty, reader) = ABIDecoder(HasWordReader.getWordReader(decodable));
    //return ABIDecode.decode(decoder, 0);
//}

// Contract Entrypoint

class ty:GenerateDispatch {
    forall f:Invokable(Unit,Unit) . function dispatch_if_selector_match(x: ty) -> f;
}

data Dispatch(name, args, retvals, f) = Dispatch(name, args, rets, f);

forall name:Selector . function selector_matches(nm : name) -> bool {
    return true;
}

instance (nm:Selector, args:ABIDecode, rets:ABIEncode, f:Invokable(args, rets)) => Dispatch(nm,args,rets,f):GenerateDispatch {
    function dispatch_if_selector_match(d:Dispatch(n,a,r,f)) -> g {
        return lam() {
            match d {
            | Dispatch(name, args, rets, fn) => match selector_matches(name) {
                | false => return Unit;
                | true =>
                  return Unit;
            };};
        };
    }
}

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
