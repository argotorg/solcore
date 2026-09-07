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
enum ContractStorage<cxt> { ContractStorage(cxt) }

enum storageRef<a> { storageRef(word) }
enum Proxy<a> { Proxy }

impl<a> Typedef<a storage, word> {
    function rep(x:a storage) returns (word) {
        match (x ) {
            case storage(y) { return y;
        } }
    }
    function abs(x:word) returns (a storage) {
        return storage(x);
    }
}

impl<a> Typedef<storageRef<a>, word> {
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

enum ref<a> { ref(a) }

impl<a> Assign<ref<a>, a> {
    function assign(l:ref<a>, r:a) returns (()) {
        // builtin "stack store"
        return;
    }
}

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
    function assign(l:storageRef<a>, y:a) returns (()) {
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

pragma solcore noPattersonCondition RValueMemberAccess; // this is due to ContractStorage(cxt); probably not needed once we have local instances
pragma solcore noCoverageCondition LValueMemberAccess, RValueMemberAccess;

impl<cxt, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>, storageRef<fieldType>> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<fieldType, offsetType>, offsetType: StorageSize {
    function memberAccess(x:MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>) returns (storageRef<fieldType>) {
        let ptr:word = 0x100; // forge uses at least 1 storage slot
        let offsetSize:word = StorageSize.size(Proxy as Proxy<offsetType>);

        assembly {
            ptr := add(ptr, offsetSize)
        }
        return storageRef(ptr); // contract storage starts at 0
    }
}

impl<cxt, fieldSelector, fieldType, offsetType> RValueMemberAccess<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>, fieldType> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<fieldType, offsetType>, fieldType: StorageType, offsetType: StorageSize {
    function memberAccess(x:MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>) returns (fieldType) {
        let ptr:word = 0x100;
        let offsetSize:word = StorageSize.size(Proxy as Proxy<offsetType>);
        return StorageType.sload(add(ptr, offsetSize)) as fieldType;
    }
}

function rval<a, b>(x:a) returns (b)  where a: RValueMemberAccess<b> {
  return RValueMemberAccess.memberAccess(x);
}
