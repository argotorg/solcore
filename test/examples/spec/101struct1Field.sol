
/////// Construction
trait Typedef<abs, rep> {
    function rep(x:abs) returns (rep);
    function abs(x:rep) returns (abs);
}


enum uint { uint(word) }

impl Typedef<word, word> {
    function rep(x:word) returns (word) { return x; }
    function abs(x:word) returns (word) { return x;}
}

impl Typedef<uint, word> {
    function rep(x:uint) returns (word) {
        match (x ) {
            case uint(y) { return y;
        } }
    }
    function abs(x:word) returns (uint) {
        return uint(x);
    }
}

enum memory<a> { memory(word) }
enum memoryRef<a> { memoryRef(word) }
enum Proxy<a> { Proxy }

impl Typedef<a memory, word> {
    function rep(x:a memory) returns (word) {
        match (x ) {
            case memory(y) { return y;
        } }
    }
    function abs(x:word) returns (a memory) {
        return memory(x);
    }
}
impl Typedef<memoryRef<a>, word> {
    function rep(x:memoryRef<a>) returns (word) {
        match (x ) {
            case memoryRef(y) { return y;
        } }
    }
    function abs(x:word) returns (memoryRef<a>) {
        return memoryRef(x);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}

enum ref<a> { ref(a) }

impl Assign<ref<a>, a> {
    function assign(l:ref<a>, r:a) returns (()) {
        // builtin "stack store"
        return;
    }
}

trait MemoryType<self> {
    function load(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
}

trait MemorySize<self> {
    function size(x:Proxy<self>) returns (word);
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

impl MemoryType<word> {
    function load(ptr:word) returns (word) {
        let r:word;
        assembly {
            r := mload(ptr)
        }
        return r;
    }
    function store(ptr:word, value:word) returns (()) {
        assembly {
            mstore(ptr, value)
        }
    }
}

impl MemoryType<uint> {
    function load(ptr:word) returns (uint) {
        return Typedef.abs(mload_(ptr)) as uint; // type annotation needed due to a typechecker bug
    }
    function store(ptr:word, value:uint) returns (()) {
        return mstore_(ptr, Typedef.rep(value));
    }
}

impl<a> Assign<memoryRef<a>, a> where a: MemoryType {
    function assign(l:memoryRef<a>, y:a) {
        MemoryType.store(Typedef.rep(l), y);
    }
}



enum MemberAccessProxy<a, field, offset> { MemberAccessProxy(a, field, Proxy<offset>) }

function memberAccessD1<a, field, offset>(x:MemberAccessProxy<a, field, offset>) returns (a) {
    match (x ) {
        case MemberAccessProxy(y,z,p) { return y;
    } }
}

trait LValueMemberAccess<self, memberRefType> {
    function memberAccess(x:self) returns (memberRefType);
}

trait RValueMemberAccess<self, memberValueType> {
    function memberAccess(x:self) returns (memberValueType);
}

// This is *a lot* of pragmas...
// pragma no-coverage-condition CStructField, LValueMemberAccess, RValueMemberAccess;
// pragma no-patterson-condition LValueMemberAccess, RValueMemberAccess;
// pragma no-bounded-variable-condition LValueMemberAccess, RValueMemberAccess;
trait CStructField<self, fieldType, offsetType> {}
enum StructField<structType, fieldSelector> { StructField(structType) }

impl<structType, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<structType memory, fieldSelector, offsetType>, memoryRef<fieldType>> where StructField<structType, fieldSelector>: CStructField<fieldType, offsetType>, offsetType: MemorySize {
    function memberAccess(x:MemberAccessProxy<structType memory, fieldSelector, offsetType>) returns (memoryRef<fieldType>) {
        let ptr:word = Typedef.rep(memberAccessD1(x));
        let size:word = MemorySize.size(Proxy as Proxy<offsetType>);
        assembly {
            ptr := add(ptr, size)
        }
        return memoryRef(ptr);
    }
}

impl MemorySize<()> {
    function size(x:Proxy<()>) returns (word) {
        return 0;
    }
}

impl MemorySize<word> {
    function size(x:Proxy<word>) returns (word) {
        return 32;
    }
}


impl MemorySize<uint> {
    function size(x:Proxy<uint>) returns (word) {
        return 32;
    }
}

impl<a, b> MemorySize<(a, b)> where a: MemorySize, b: MemorySize {
    function size(x:Proxy<(a, b)>) returns (word) {
        let a_sz:word = MemorySize.size(Proxy as Proxy<a>);
        let b_sz:word = MemorySize.size(Proxy as Proxy<b>);
        assembly {
            a_sz := add(a_sz, b_sz)
        }
        return a_sz;
    }
}

impl<structType, fieldSelector, fieldType, offsetType> RValueMemberAccess<MemberAccessProxy<structType memory, fieldSelector, offsetType>, fieldType> where StructField<structType, fieldSelector>: CStructField<fieldType, offsetType>, fieldType: MemoryType, offsetType: MemorySize {
    function memberAccess(x:MemberAccessProxy<structType memory, fieldSelector, offsetType>) returns (fieldType) {
        let ptr:word = Typedef.rep(memberAccessD1(x));
        let size:word = MemorySize.size(Proxy as Proxy<offsetType>);
        assembly {
            ptr := add(ptr, size)
        }
        return MemoryType.load(ptr);
    }
}

////// Testing

// struct S { fld1:word; }
enum S { S(word) }
enum fld1_sel { fld1_sel }
// data y_sel = y_sel;
// data z_sel = z_sel;

impl CStructField<StructField<S, x_sel>, word, ()> {}
// instance StructField(S, y_sel):CStructField(uint, word) {}
// BUG: This next one should really be the following, but that breaks weirdly:
// (I get a patterson condition violation on an invoke instance for g)
impl CStructField<StructField<S, z_sel>, word, (word, uint)> {}
// So instead I use:
// instance StructField(S, z_sel):CStructField(word, word) {}


function f() {
    let x:word memory;
    let y:word memory;
    // x = y
    Assign.assign(ref(x), y);
    /*
     * Idea in the above: to avoid overlapping instances,
     * we can desugar a simple identifier referring to a local variable on the lhs of an assignment to ref(x),
     * to be able to choose a disjoint assign instance.
     * Of course this needs special treatment during code generation,
     * on the other hand, stack assignments generally do...
     * Actually, even simpler might be just *not* to desugar assignments at all, if the lhs is just an identifier referring to a local variable and just directly take care of it when translating to core.
     */
}

function g() returns (word) {
    let s:S memory = Typedef.abs(0x80);

    let offset0 : Proxy<()> = Proxy;
    // s.fld1 = y
    let fld1_lval : memoryRef<word>
                  = LValueMemberAccess.memberAccess(MemberAccessProxy(s, fld1_sel, offset0));
    Assign.assign(fld1_lval, y);
    // return s.fld1
    let r : word = 17;
    r = RValueMemberAccess.memberAccess(MemberAccessProxy(s, fld1_sel, offset0) );
    return r;
}

contract C {
    function main() public {
        f();
        return g();
    }
}
