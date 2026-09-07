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

enum storage<a> { storage(word) }
enum storageRef<a> { storageRef(word) }
enum Proxy<a> { Proxy }

impl Typedef<a storage, word> {
    function rep(x:a storage) returns (word) {
        match (x ) {
            case storage(y) { return y;
        } }
    }
    function abs(x:word) returns (a storage) {
        return storage(x);
    }
}
impl Typedef<storageRef<a>, word> {
    function rep(x:storageRef<a>) returns (word) {
        match (x ) {
            case storageRef(y) { return y;
        } }
    }
    function abs(x:word) returns (storageRef<a>) {
        return storageRef(x);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}

/*
data ref(a) = ref(a);

instance ref(a):Assign(a) {
    function assign(l:ref(a), r:a) -> () {
        // builtin "stack store"
        return ();
    }
}
*/

trait StorageType<self> {
    function sload(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
}

trait StorageSize<self> {
    function size(x:Proxy<self>) returns (word);
}


function sload_(x:word) returns (word) {
    let res: word;
    assembly {
       res := sload(x)
    }
    return res;
  }

function sstore_(a:word, v:word) {
    assembly { sstore(a,v) }
}

impl StorageType<word> {
    function sload(ptr:word) returns (word) {
        let r:word;
        assembly {
            r := sload(ptr)
        }
        return r;
    }
    function store(ptr:word, value:word) returns (()) {
        assembly {
            sstore(ptr, value)
        }
    }
}

impl StorageType<uint> {
    function sload(ptr:word) returns (uint) {
        return Typedef.abs(sload_(ptr)) as uint; // type annotation needed due to a typechecker bug
    }
    function store(ptr:word, value:uint) returns (()) {
        return sstore_(ptr, Typedef.rep(value));
    }
}

impl<a> Assign<storageRef<a>, a> where a: StorageType {
    function assign(l:storageRef<a>, y:a) {
        StorageType.store(Typedef.rep(l), y);
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

impl<structType, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<structType storage, fieldSelector, offsetType>, storageRef<fieldType>> where StructField<structType, fieldSelector>: CStructField<fieldType, offsetType>, offsetType: StorageSize {
    function memberAccess(x:MemberAccessProxy<structType storage, fieldSelector, offsetType>) returns (storageRef<fieldType>) {
        let ptr:word = Typedef.rep(memberAccessD1(x));
        let size:word = StorageSize.size(Proxy as Proxy<offsetType>);
        assembly {
            ptr := add(ptr, size)
        }
        return storageRef(ptr);
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

impl StorageSize<uint> {
    function size(x:Proxy<uint>) returns (word) {
        return 1;
    }
}


/*
// fails Patterson cond
forall a b . a:Typedef(b), b:StorageSize
=> instance a:StorageSize {
    function size(x:Proxy(a)) -> word {
        return StorageSize.size(Proxy(b));
    }
}
*/

impl<a, b> StorageSize<(a, b)> where a: StorageSize, b: StorageSize {
    function size(x:Proxy<(a, b)>) returns (word) {
        let a_sz:word = StorageSize.size(Proxy as Proxy<a>);
        let b_sz:word = StorageSize.size(Proxy as Proxy<b>);
        assembly {
            a_sz := add(a_sz, b_sz)
        }
        return a_sz;
    }
}

impl<structType, fieldSelector, fieldType, offsetType> RValueMemberAccess<MemberAccessProxy<structType storage, fieldSelector, offsetType>, fieldType> where StructField<structType, fieldSelector>: CStructField<fieldType, offsetType>, fieldType: StorageType, offsetType: StorageSize {
    function memberAccess(x:MemberAccessProxy<structType storage, fieldSelector, offsetType>) returns (fieldType) {
        let ptr:word = Typedef.rep(memberAccessD1(x));
        let size:word = StorageSize.size(Proxy as Proxy<offsetType>);
        assembly {
            ptr := add(ptr, size)
        }
        return StorageType.sload(ptr) as fieldType;
    }
}

// helpers

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
    let s:S storage = Typedef.abs(0x80);
    let fld1_map : MemberAccessProxy<S storage, fld1_sel, ()> = MemberAccessProxy(s, fld1_sel);
    let fld2_map : MemberAccessProxy<S storage, fld2_sel, uint> = MemberAccessProxy(s, fld2_sel);
    let fld3_map = MemberAccessProxy(s, fld3_sel)
		  as MemberAccessProxy<S storage, fld3_sel, (uint, word)>;
    // let y:word = 13;
    let z:uint = uint(13);

    // s.fld1 = z

    let fld1_lval : storageRef<uint>
                  = LValueMemberAccess.memberAccess(fld1_map );
    Assign.assign(fld1_lval, z);

    // s.fld2 = 14
    let fld2_lval // : storageRef(word)
                  = LValueMemberAccess.memberAccess(fld2_map);
    Assign.assign(fld2_lval, 14);

    // s.fld3 = 15
    let fld3_lval // : storageRef(word)
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
