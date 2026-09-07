// Before desugaring:
/*
import IndexLib;

contract Uint {
  reserved : word;
  owner : address;
  decimals : uint;
  totalSupply : uint;
  balances : mapping(address,uint);

  function mint(amount:uint) {
    balances[owner] = Num.add(balances[owner], amount);
    totalSupply = Num.add(totalSupply, amount);
  }

  function init() {
    owner = address(0x123456789abcdef);
    decimals = Num.fromWord(18);
  }
  function main() -> uint {
    init();
    mint(uint(1000));
    mint(uint(1000));
    return balances[owner] : uint;
  }
}
*/


function addW(x : word, y : word) returns (word) {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

function subW(x : word, y : word) returns (word) {
  let res: word;
  assembly {
     res := sub(x, y)
  }
  return res;
}

function addU(x : uint, y : uint) returns (uint) {
  let res: word;
  let xw : word = Num.toWord(x);
  let yw : word = Num.toWord(y);
  assembly {
     res := add(xw, yw)
  }
  return uint(res);
}

function hash1(x: word) returns (word) {
  let result: word = 0;
  assembly {
    mstore(0, x)
    result := keccak256(0,32)
  }
  return result;
}

function hash2(x: word, y: word) returns (word) {
  let result: word = 0;
  assembly {
    mstore(0, x)
    mstore(32, y)
    result := keccak256(0,64)
  }
  return result;
}

trait Num<a> {
  function toWord(x:a) returns (word);
  function fromWord(x:word) returns (a);
  function add(x:a, y:a) returns (a);
  function sub(x:a, y:a) returns (a);
}

impl Num<word> {
  function toWord(x:word) returns (word) { return x; }
  function fromWord(x:word) returns (word) { return x; }
  function add(x:word, y:word) returns (word) { return addW(x, y); }
  function sub(x:word, y:word) returns (word) { return addW(x, y); }
}

enum uint { uint(word) }

impl Num<uint> {
  function toWord(x:uint) returns (word)
  {
          match (x ) {
            case uint(y) { return y;
        } }
  }

  function fromWord(x:word) returns (uint) { return uint(x); }
  function add(x:uint, y:uint) returns (uint) { return uint(addW(Num.toWord(x), Num.toWord(y))); }
  function sub(x:uint, y:uint) returns (uint) { return uint(subW(Num.toWord(x), Num.toWord(y))); }
}

/* // this breaks the Paterson condition
forall a. a:Typedef(word) =>
instance a:Num {
  function toWord(x:a) -> word { return Typedef.rep(x); }
  function fromWord(x:word) { return Typedef.abs(x); }
  function add(x:a, y:a) -> a { return Typedef.abs(addW(Typedef.rep(x), Typedef.rep(y))); }
}
*/

// Storage slots and mapping access


/////// Construction
trait Typedef<abs, rep> {
    function rep(x:abs) returns (rep);
    function abs(x:rep) returns (abs);
}


// this does not work :(
/*
forall a
. default instance a:Typedef(a) {
    function rep(x:a) -> word { return a; }
    function abs(x:a) -> word { return a;}
}
*/

impl Typedef<word, word> {
    function rep(x:word) returns (word) { return x; }
    function abs(x:word) returns (word) { return x; }
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

enum storage<a> { storage(word) }
enum ContractStorage<cxt> { ContractStorage(cxt) }

enum storageRef<a> { storageRef(word) }
enum Proxy<a> { Proxy }

enum mapRef<a> { mapRef(word) } //ref to a map elem

// data memoryRef(a) = memoryRef(word);

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

function sstore_(a:word, v:word) returns (()) {
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

impl StorageType<address> {
    function sload(ptr:word) returns (address) {
        return Typedef.abs(sload_(ptr)) as address; // type annotation needed due to a typechecker bug
    }
    function store(ptr:word, value:address) returns (()) {
        return sstore_(ptr, Typedef.rep(value));
    }
}

impl<a> Assign<storageRef<a>, a> where a: StorageType {
    function assign(l:storageRef<a>, y:a) returns (()) {
        StorageType.store(Typedef.rep(l), y);
    }
}

trait CStructField<self, fieldType, offsetType> {}
enum StructField<structType, fieldSelector> { StructField(structType) }


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

impl StorageSize<address> {
    function size(x:Proxy<address>) returns (word) {
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
pragma solcore noCoverageCondition MemberAccessProxy, LValueMemberAccess, RValueMemberAccess;

// ------------------------------------------------------------------
// Contract field access
// ------------------------------------------------------------------

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
        return StorageType.sload(addW(ptr, offsetSize)) as fieldType;
    }
}

/*
forall cxt fieldSelector fieldType offsetType
  . StructField(ContractStorage(cxt), fieldSelector):CStructField(fieldType, offsetType)
  , fieldType:StorageType
  , offsetType:StorageSize
  => instance MemberAccessProxy(ContractStorage(cxt), fieldSelector, offsetType):RValueMemberAccess(fieldType) {
    function memberAccess(x:MemberAccessProxy(ContractStorage(cxt), fieldSelector, offsetType)) -> fieldType {
        let ptr:word = 0x100;
        let offsetSize:word = StorageSize.size(Proxy:Proxy(offsetType));
        return StorageType.sload(addW(ptr, offsetSize)):fieldType;
    }
}
*/
// ------------------------------------------------------------------
// Indexed access
// ------------------------------------------------------------------

enum mapping<index, member> { mapping(word) }

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

enum IndexAccessProxy<map, index, member> { IndexAccessProxy(map, index) }

impl<map, index, member> LValueMemberAccess<IndexAccessProxy<map, index, member>, storageRef<member>> where index: Typedef<word>, map: Typedef<word> {
    function memberAccess(x:IndexAccessProxy<map, index, member>) returns (storageRef<member>) {
	    return storageRef(indexStorageSlot(x));
    }
}

impl<map, index, member> RValueMemberAccess<IndexAccessProxy<map, index, member>, member> where index: Typedef<word>, member: StorageType, map: Typedef<word> {
    function memberAccess(x:IndexAccessProxy<map, index, member>) returns (member) {
            let slot:word = indexStorageSlot(x);
	    return StorageType.sload(slot);
    }
}

function indexStorageSlot<index, map, member>(x:IndexAccessProxy<map, index, member>) returns (word)
//function indexStorageSlot(x)
 where map: Typedef<word>, index: Typedef<word> {
	match (x ) {
	    case IndexAccessProxy(map, i) {
	    	    let mapptr:word = Typedef.rep(map);
                    let rawidx:word = Typedef.rep(i);
		    let loc:word = hash2(mapptr, rawidx);
		    return loc;
        } }
}

/*
forall index map member. map:Typedef(word), index:Typedef(word)
=> function indexedSlot(mapref : storageRef(mapping(index, member)), i: index) -> word
{
	match mapref {
	    | storageRef(mapptr) =>
                    let rawidx:word = Typedef.rep(i);
		    let loc:word = hash2(mapptr, rawidx);
		    return loc;
        }
}
*/

function rval<a, b>(x:a) returns (b)  where a: RValueMemberAccess<b> {
  return RValueMemberAccess.memberAccess(x);
}

enum UintCxt { UintCxt }
enum reserved_sel { reserved_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, reserved_sel>, word, ()> {
}
enum owner_sel { owner_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, owner_sel>, address, (word, ())> {
}
enum decimals_sel { decimals_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, decimals_sel>, uint, (word, (address, ()))> {
}
enum totalSupply_sel { totalSupply_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, totalSupply_sel>, uint, (word, (address, (uint, ())))> {
}
enum balances_sel { balances_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, balances_sel>, mapping(address => uint), (word, (address, (uint, (uint, ()))))> {
}
contract Uint {
   function mint (amount : uint) public returns (()) {
      Assign.assign(LValueMemberAccess.memberAccess(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)))), Num.add(rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)))), amount));
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), totalSupply_sel)), Num.add(rval(MemberAccessProxy(ContractStorage(UintCxt), totalSupply_sel)), amount));
   }
   function init () public returns (()) {
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)), address(81985529216486895));
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), decimals_sel)), Num.fromWord(18));
   }
   function main () public returns (uint) {
      init();
      mint(uint(1000));
      mint(uint(1000));
      return rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)))) as uint;
   }
}

