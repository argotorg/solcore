// v4: Simplified Member AccessProxy (no Proxy(offset))
// variables holding field MAPs

function add(x : word, y : word) {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

/////// Construction
trait Typedef<abs, rep> {
    function rep(x:abs) returns (rep);
    function abs(x:rep) returns (abs);
}


enum uint { uint(word) }

// this does not work :(
/*
forall a
. default instance a:Typedef(a) {
    function rep(x:a) -> word { return a; }
    function abs(x:a) -> word { return a;}
}
*/

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
        return Typedef.abs(mload_(ptr)) as uint;
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



enum MemberAccessProxy<a, field, offset> { MemberAccessProxy(a, field) }

function memberAccessD1<a, field, offset>(x:MemberAccessProxy<a, field, offset>) returns (a) {
    match (x ) {
        case MemberAccessProxy(y,z) { return y;
    } }
}

trait LValueMemberAccess<self, memberRefType> {
    function memberAccess(x:self) returns (memberRefType);
}

trait RValueMemberAccess<self, memberValueType> {
    function memberAccess(x:self) returns (memberValueType);
}

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


/*
// fails Patterson cond
forall a b . a:Typedef(b), b:MemorySize
=> instance a:MemorySize {
    function size(x:Proxy(a)) -> word {
        return MemorySize.size(Proxy(b));
    }
}
*/

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
        return MemoryType.load(ptr) as fieldType;
    }
}

////// Testing

// struct S { fld1:uint; fld2:word; fld3:word }
enum S { S } // (uint, word, word);
enum fld1_sel { fld1_sel }
enum fld2_sel { fld2_sel }
enum fld3_sel { fld3_sel }

// form:
// instance StructField(S, f_sel):CStructField(ftype, preceding)) {}
impl CStructField<StructField<S, fld1_sel>, uint, ()> {}
impl CStructField<StructField<S, fld2_sel>, word, uint> {}
impl CStructField<StructField<S, fld3_sel>, word, (uint, word)> {}


function g() returns (word) {
    let s:S memory = Typedef.abs(0x80);
    let fld1_map : MemberAccessProxy<S memory, fld1_sel, ()> = MemberAccessProxy(s, fld1_sel);
    let fld2_map : MemberAccessProxy<S memory, fld2_sel, uint> = MemberAccessProxy(s, fld2_sel);
    let fld3_map = MemberAccessProxy(s, fld3_sel)
		  as MemberAccessProxy<S memory, fld3_sel, (uint, word)>;
    // let y:word = 13;
    let z:uint = uint(13);

    // s.fld1 = z

    let fld1_lval : memoryRef<uint>
                  = LValueMemberAccess.memberAccess(fld1_map );
    Assign.assign(fld1_lval, z);

    // s.fld2 = 14
    let fld2_lval // : memoryRef(word)
                  = LValueMemberAccess.memberAccess(fld2_map);
    Assign.assign(fld2_lval, 14);

    // s.fld3 = 15
    let fld3_lval // : memoryRef(word)
                  = LValueMemberAccess.memberAccess(fld3_map);
    Assign.assign(fld3_lval, 15);

    // let f1 = S.fld1
    let f1 : uint;
    f1 = RValueMemberAccess.memberAccess(fld1_map);

    let f2 : word;
    f2 = RValueMemberAccess.memberAccess(fld2_map);

    let f3 : word;
    f3 = RValueMemberAccess.memberAccess(fld3_map);

    let f12 = add(Typedef.rep(f1) as word, f2);
    let f123 = add(f12, f3);

    return f123;

}

contract C {
    function main() public {
        return g();
    }
}
