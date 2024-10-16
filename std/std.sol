/*
   Assumptions
   - We don't have built in literals for Yul Words
*/


data Bool = True | False;
function and(x: Bool, y: Bool) -> Bool {
    match x, y {
    | True, y => return y;
    | False, _ => return False;
    };
}

function or(x: Bool, y: Bool) -> Bool {
    match x, y {
    | True, _ => return True;
    | False, y => return y;
    };
}


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

// Basics

data Proxy(t) = Proxy;

// References

data Calldata(t) = Calladata(Word)
data Memory(t) = Memory(Word)
data Storage(t) = Storage(Word)
data Returndata(t) = ReturnData(Word)
data Stack(t) = Stack(Word)

// Type Classification

// types that can be completely represented by a single stack slot
class t:ValueTy {
    function abs(x:Word) -> t;
    function rep(x:t) -> Word;
}

instance Memory(t) : ValueTy {
    function abs(x: Word) -> Memory(T) {
        return Memory(x);
    }

    function rep(x: Memory(T)) -> Word {
        match x {
        | Memory(w) => return w;
       };
    }
}

class ref:Ref(deref) {
    function load(loc: ref) -> deref;
    function store(loc: ref, value: deref) -> Unit;
}


instance Memory(t) : Ref(t) {
    function load(loc: Memory(t)) -> t {
        let rw = rep(loc);
        let res = 0;
        assembly {
            res := mload(rw)
        };
        return abs(res);

    }

    function store(loc: Memory(t), value: t) -> Unit {
        let rw = rep(loc);
        let vw = rep(value);
        assembly {
          mstore(rw, vw)
        };
    }
}



// Memory Layout

function get_free_memory() -> Word {
    let res : Word;
    assembly {
        res := mload(0x40)
    };
    return res;
}

function set_free_memory(loc : Word) {
    assembly {
        mstore(0x40, loc)
    };
}


// Bytes

data Bytes = Bytes
// TODO: bytes should be indexable (but not ref since it can't live on stack...)

// ABI Encoding

class t:EncodeInto {
    function encodeInto(val : t, hd : Word, tl : Word) -> Pair(Word,Word);
}

class t:Encode {
    // TODO: is the following ever needed??
    // is the abi representation of `t` dynamically sized?
    // function isDynamicallySized(x:Proxy(t)) -> Bool;

    // does `t` (or any of it's children) contain a dynamic type?
    function shouldEncodeDynamic(x:Proxy(t)) -> Bool;
    // how big is the head portion of the ABI representation of `t`?
    function headSize(x:Proxy(t)) -> Word;
}

function encode(val:t) -> Memory(Bytes) {
    let hdSz = headSize(Proxy);
    let ptr = get_free_memory();
    let head: Word;
    let tail: Word;
    assembly {
        head := add(ptr, 32);
        tail := add(head, hdSz);
    };
    let tl = snd(encodeInto(val,head,tail));
    set_free_memory(tl);
    assembly {
        mstore(ptr, sub(tl, head))
    };
    return abs(ptr);
}

instance (l:Encode, r:Encode) => Pair(l,r):Encode {
    function shouldEncodeDynamic(x : Proxy(Pair(l,r))) -> Bool {
        let l: Proxy(l) = Proxy;
        let r: Proxy(r) = Proxy;
        return and(shouldEncodeDynamic(l), shouldEncodeDynamic(l));
    }

    function headSize(x : Proxy(Pair(l,r))) -> Word {
        match shouldEncodeDynamic(x) {
        | True =>
            let res;
            assembly { res := 32; };
            return res;
        | False =>
            let l: Proxy(l) = Proxy;
            let r: Proxy(r) = Proxy;
            let lsize = headSize(l);
            let rsize = headSize(r);
            let res;
            assembly { res := add(lsize,rsize) };
            return res;
        };
    }
}

instance (l:EncodeInto, r:EncodeInto) => Pair(l,r):EncodeInto {
    function encodeInto(val, hd, tl) -> Pair(Word,Word) {
        let first = fst(val);
        let second = snd(val);
        let range = encodeInto(first, hd, tl);
        return encodeInto(second, fst(range), snd(range));
    }
}

// Uint256
data Uint256 = Uint256(Word)
instance Uint256 : ValueTy {
    function abs(x:Word) -> Uint256 {
        return Uint256(x);
    }

    function rep(x: Uint256) -> Word {
        match x {
        | Uint256(val) => return val;
        };
    }
}

instance Uint256:Encode {
    function shouldEncodeDynamic(x) -> Bool {
        return False;
    }

    function headSize(x) -> Word {
        return 32;
    }
}

instance Uint256:EncodeInto {
    function encodeInto(v: Uint256, hd: Word, tl: Word) -> Pair(Word, Word) {
        let vw = rep(v);
        let hd_;
        assembly {
            hd_ := add(hd, 32)
            mstore(hd, vw)
        };
        return Pair(hd_, tl);
    }
}


// Contract Entrypoint

// TODO: need to have a way to write predicate in class function or class itself, or have a function type.
// class self:GenerateDispatch {
//     function  dispatch_if_selector_match(x: self) -> (Unit -> Unit);
// }
//
// data Dispatch(name,args,retvals) = DispatchFunction name (args->retvals)
//
// instance args:ABI => retvals:ABI => name:Selector => dispatchFuncion[name,args,retvals]:GenerateDispatch {
//     function dispatch_if_selector_match(self:dispatchFuncion[name,args,retvals]) -> (() -> ()) {
//         return lambda () {
//             let DispatchFunction(name,f) := self; // or whatever destructuring syntax we want
//             if matches_selector(name) // checks selector in calldata
//             {
//                 let (result_start, result_end) = abi.encode(f(abi.decode(TYPE(retvals)))); // conceptually at least
//                 assembly {
//                     return(result_start, result_end); // as in EVM return, terminating the external call.
//                 }
//             }
//         };
//     }
// }
//
// instance a:GenerateDispatch => b:GenerateDispatch => Pair[a,b]:GenerateDispatch {
//     function dispatch_if_selector_match(self:Pair[a,b]) -> (() -> ()) {
//         return lambda () . {
//             dispatch_if_selector_match(self.first);
//             dispatch_if_selector_match(self.second);
//         };
//     }
// }
//
// /// Translation of the above contract
// struct StorageContext {
//     x:uint;
//     y:bool;
// }
//
// function C_f(ctxt:StorageContext) public {
//     ctxt.x = 42;
// }
//
//
// function entry_C() {
//     GenerateDispatch.dispatch_if_selector_match(DispatchFunction("f()", C_f)); // could also be (nested) pairs of dispatch functions, if the contract had more functions
//     revert("unknown selector");
// }
//
// // init code for contract creation
// function init_C() {
//     // constructor code
//     let code_start := allocate_unbounded() // fetch some free memory
//     let code_length := __builtin_fetch_code(entry_C, code_start) // sounds weirder than it is - this will just add the code for entry_C to a Yul subobject and use some Yul builtins for fetching the code to be deployed
//     assembly {
//         return(code_start, code_length)
//     }
// }
//
//
