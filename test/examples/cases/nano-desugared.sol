function addW (x : word, y : word) {
   let res : word ;
   assembly { res := add(x, y)
            }
   return res;
}
function subW (x : word, y : word) {
   let res : word ;
   assembly { res := sub(x, y)
            }
   return res;
}
function addU (x : uint, y : uint) returns (uint) {
   let res : word ;
   let xw : word = Num.toWord(x) ;
   let yw : word = Num.toWord(y) ;
   assembly { res := add(xw, yw)
            }
   return uint(res);
}
function hash1 (x : word) returns (word) {
   let result : word = 0 ;
   assembly { mstore(0, x)
              result := keccak256(0, 32)
            }
   return result;
}
function hash2 (x : word, y : word) returns (word) {
   let result : word = 0 ;
   assembly { mstore(0, x)
              mstore(32, y)
              result := keccak256(0, 64)
            }
   return result;
}
enum Bool { False, True }
function not (b : Bool) returns (Bool) {
   match (b) {
   case Bool.False {
      return Bool.True;
   } case Bool.True {
      return Bool.False;
   } }
}
function or (x : Bool, y : Bool) returns (Bool) {
   match (x) {
   case Bool.False {
      return y;
   } case Bool.True {
      return Bool.True;
   } }
}
function fromBool (b) {
   match (b) {
   case Bool.False {
      return 0;
   } case Bool.True {
      return 1;
   } }
}
function toBool (x : word) {
   match (x) {
   case 0 {
      return Bool.False;
   } default {
      return Bool.True;
   } }
}
trait Num<a> {
   function toWord (x : a) returns (word);
   function fromWord (x : word) returns (a);
   function add (x : a, y : a) returns (a);
   function sub (x : a, y : a) returns (a);
   function eq (x : a, y : a) returns (Bool);
   function gt (x : a, y : a) returns (Bool);
}
impl Num<word> {
   function toWord (x : word) returns (word) {
      return x;
   }
   function fromWord (x : word) returns (word) {
      return x;
   }
   function add (x : word, y : word) returns (word) {
      return addW(x, y);
   }
   function sub (x : word, y : word) returns (word) {
      return addW(x, y);
   }
   function eq (x : word, y : word) returns (Bool) {
      let res : word ;
      assembly { res := eq(x, y)
               }
      return toBool(res);
   }
   function gt (x : word, y : word) returns (Bool) {
      let res : word ;
      assembly { res := gt(x, y)
               }
      return toBool(res);
   }
}
function ge<a> (x : a, y : a) returns (Bool)  where a: Num {
   return or(Num.gt(x, y), Num.eq(x, y));
}
enum uint { uint(word) }
impl Num<uint> {
   function toWord (x : uint) returns (word) {
      match (x) {
      case uint(y) {
         return y;
      } }
   }

   function fromWord (x : word) returns (uint) {
      return uint(x);
   }
   function add (x : uint, y : uint) returns (uint) {
      return uint(addW(Num.toWord(x), Num.toWord(y)));
   }
   function sub (x : uint, y : uint) returns (uint) {
      return uint(subW(Num.toWord(x), Num.toWord(y)));
   }
   function eq (x : uint, y : uint) returns (Bool) {
      return Num.eq(Num.toWord(x), Num.toWord(y));
   }
   function gt (x : uint, y : uint) returns (Bool) {
      return Num.gt(Num.toWord(x), Num.toWord(y));
   }
}
trait Typedef<abs, rep> {
   function rep (x : abs) returns (rep);
   function abs (x : rep) returns (abs);
}
impl Typedef<word, word> {
   function rep (x : word) returns (word) {
      return x;
   }
   function abs (x : word) returns (word) {
      return x;
   }
}
impl Typedef<uint, word> {
   function rep (x : uint) returns (word) {
      match (x) {
      case uint(y) {
         return y;
      } }
   }
   function abs (x : word) returns (uint) {
      return uint(x);
   }
}
enum address { address(word) }
impl Typedef<address, word> {
   function rep (x : address) returns (word) {
      match (x) {
      case address(y) {
         return y;
      } }
   }
   function abs (x : word) returns (address) {
      return address(x);
   }
}
enum storage<a> { storage(word) }
enum ContractStorage<cxt> { ContractStorage(cxt) }
enum storageRef<a> { storageRef(word) }
enum Proxy<a> { Proxy }
enum mapping<member, index> { mapping(word, Proxy<member>, Proxy<index>) }
enum mapRef<a> { mapRef(word) }
impl<a> Typedef<a storage, word> {
   function rep (x : a storage) returns (word) {
      match (x) {
      case storage(y) {
         return y;
      } }
   }
   function abs (x : word) returns (a storage) {
      return storage(x);
   }
}
impl<a> Typedef<storageRef<a>, word> {
   function rep (x : storageRef<a>) returns (word) {
      match (x) {
      case storageRef(y) {
         return y;
      } }
   }
   function abs (x : word) returns (storageRef<a>) {
      return storageRef(x);
   }
}
trait Assign<lhs, rhs> {
   function assign (l : lhs, r : rhs) returns (());
}
enum ref<a> { ref(a) }
impl<a> Assign<ref<a>, a> {
   function assign (l : ref<a>, r : a) returns (()) {
      return;
   }
}
trait StorageType<self> {
   function sload (ptr : word) returns (self);
   function store (ptr : word, value : self) returns (());
}
trait StorageSize<self> {
   function size (x : Proxy<self>) returns (word);
}
function sload_ (x : word) returns (word) {
   let res : word ;
   assembly { res := sload(x)
            }
   return res;
}
function sstore_ (a : word, v : word) {
   assembly { sstore(a, v)
            }
}
impl StorageType<word> {
   function sload (ptr : word) returns (word) {
      let r : word ;
      assembly { r := sload(ptr)
               }
      return r;
   }
   function store (ptr : word, value : word) returns (()) {
      assembly { sstore(ptr, value)
               }
   }
}
impl StorageType<uint> {
   function sload (ptr : word) returns (uint) {
      return Typedef.abs(sload_(ptr)) as uint;
   }
   function store (ptr : word, value : uint) returns (()) {
      return sstore_(ptr, Typedef.rep(value));
   }
}
impl StorageType<address> {
   function sload (ptr : word) returns (address) {
      return Typedef.abs(sload_(ptr)) as address;
   }
   function store (ptr : word, value : address) returns (()) {
      return sstore_(ptr, Typedef.rep(value));
   }
}
impl<a> Assign<storageRef<a>, a> where a: StorageType {
   function assign (l : storageRef<a>, y : a) returns (()) {
      StorageType.store(Typedef.rep(l), y);
   }
}
trait CStructField<self, fieldType, offsetType> {
}
enum StructField<structType, fieldSelector> { StructField(structType) }
enum MemberAccessProxy<a, field, offset> { MemberAccessProxy(a, field) }
function memberAccessD1<a, field, offset> (x : MemberAccessProxy<a, field, offset>) returns (a) {
   match (x) {
   case MemberAccessProxy(y, z) {
      return y;
   } }
}
trait LValueMemberAccess<self, memberRefType> {
   function memberAccess (x : self) returns (memberRefType);
}
trait RValueMemberAccess<self, memberValueType> {
   function memberAccess (x : self) returns (memberValueType);
}
impl<structType, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<structType storage, fieldSelector, offsetType>, storageRef<fieldType>> where StructField<structType, fieldSelector>: CStructField<fieldType, offsetType>, offsetType: StorageSize {
   function memberAccess (x : MemberAccessProxy<structType storage, fieldSelector, offsetType>) returns (storageRef<fieldType>) {
      let ptr : word = Typedef.rep(memberAccessD1(x)) ;
      let size : word = StorageSize.size(Proxy as Proxy<offsetType>) ;
      assembly { ptr := add(ptr, size)
               }
      return storageRef(ptr);
   }
}
impl StorageSize<()> {
   function size (x : Proxy<()>) returns (word) {
      return 0;
   }
}
impl StorageSize<word> {
   function size (x : Proxy<word>) returns (word) {
      return 1;
   }
}
impl StorageSize<uint> {
   function size (x : Proxy<uint>) returns (word) {
      return 1;
   }
}
impl StorageSize<address> {
   function size (x : Proxy<address>) returns (word) {
      return 1;
   }
}
impl<a, b> StorageSize<(a, b)> where a: StorageSize, b: StorageSize {
   function size (x : Proxy<(a, b)>) returns (word) {
      let a_sz : word = StorageSize.size(Proxy as Proxy<a>) ;
      let b_sz : word = StorageSize.size(Proxy as Proxy<b>) ;
      assembly { a_sz := add(a_sz, b_sz)
               }
      return a_sz;
   }
}
impl<cxt, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>, storageRef<fieldType>> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<fieldType, offsetType>, offsetType: StorageSize {
   function memberAccess (x : MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>) returns (storageRef<fieldType>) {
      let ptr : word = 256 ;
      let offsetSize : word = StorageSize.size(Proxy as Proxy<offsetType>) ;
      assembly { ptr := add(ptr, offsetSize)
               }
      return storageRef(ptr);
   }
}
impl<cxt, fieldSelector, fieldType, offsetType> RValueMemberAccess<MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>, fieldType> where StructField<ContractStorage<cxt>, fieldSelector>: CStructField<fieldType, offsetType>, fieldType: StorageType, offsetType: StorageSize {
   function memberAccess (x : MemberAccessProxy<ContractStorage<cxt>, fieldSelector, offsetType>) returns (fieldType) {
      let ptr : word = 256 ;
      let offsetSize : word = StorageSize.size(Proxy as Proxy<offsetType>) ;
      return StorageType.sload(addW(ptr, offsetSize)) as fieldType;
   }
}
enum mapping<index, member> { mapping(word) }
impl<member, index> Typedef<mapping(index => member), word> {
   function rep (x : mapping(index => member)) returns (word) {
      match (x) {
      case mapping(y) {
         return y;
      } }
   }
   function abs (x : word) returns (mapping(index => member)) {
      return mapping(x);
   }
}
impl<index, member> StorageSize<mapping(index => member)> {
   function size (x : Proxy<mapping(index => member)>) returns (word) {
      return 1;
   }
}
enum IndexAccessProxy<map, index, member> { IndexAccessProxy(map, index) }
impl<index, member> LValueMemberAccess<IndexAccessProxy<storageRef<mapping(index => member)>, index, member>, storageRef<member>> where index: Typedef<word> {
   function memberAccess (x : IndexAccessProxy<storageRef<mapping(index => member)>, index, member>) returns (storageRef<member>) {
      return storageRef(indexStorageSlot(x));
   }
}
impl<map, index, member> RValueMemberAccess<IndexAccessProxy<map, index, member>, member> where index: Typedef<word>, member: StorageType, map: Typedef<word> {
   function memberAccess (x : IndexAccessProxy<map, index, member>) returns (member) {
      let slot : word = indexStorageSlot(x) ;
      return StorageType.sload(slot);
   }
}
function indexStorageSlot<index, map, member> (x : IndexAccessProxy<map, index, member>) returns (word)  where map: Typedef<word>, index: Typedef<word> {
   match (x) {
   case IndexAccessProxy(map, i) {
      let mapptr : word = Typedef.rep(map) ;
      let rawidx : word = Typedef.rep(i) ;
      let loc : word = hash2(mapptr, rawidx) ;
      return loc;
   } }
}
function rval<a, b> (x : a) returns (b)  where a: RValueMemberAccess<b> {
   return RValueMemberAccess.memberAccess(x);
}
function caller () returns (address) {
   let res : word ;
   assembly { res := caller()
            }
   return address(res);
}
function require1fail () {
   let res : word ;
   assembly { mstore(0, 2320231852978620534530211544385868)
              revert(0, 32)
            }
   return;
}
function require1 (cond : Bool) {
   match (cond) {
   case Bool.False {
      return require1fail();
   } case Bool.True {
      return;
   } }
}
function nop () returns (()) {
   return;
}
enum UintCxt { UintCxt }
enum reserved_sel { reserved_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, reserved_sel>, word, ()> {
}
enum msg_sender_sel { msg_sender_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, msg_sender_sel>, address, (word, ())> {
}
enum owner_sel { owner_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, owner_sel>, address, (word, (address, ()))> {
}
enum decimals_sel { decimals_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, decimals_sel>, uint, (word, (address, (address, ())))> {
}
enum totalSupply_sel { totalSupply_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, totalSupply_sel>, uint, (word, (address, (address, (uint, ()))))> {
}
enum balances_sel { balances_sel }
impl CStructField<StructField<ContractStorage<UintCxt>, balances_sel>, mapping(address => uint), (word, (address, (address, (uint, (uint, ())))))> {
}
contract Uint {
   function mint (amount : uint) public {
      Assign.assign(LValueMemberAccess.memberAccess(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)))), Num.add(rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)))), amount));
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), totalSupply_sel)), Num.add(rval(MemberAccessProxy(ContractStorage(UintCxt), totalSupply_sel)), amount));
   }
   function transferFrom (src : address, dst : address, amt : uint) public returns (Bool) {
      require1(ge(rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), src)), amt));
      withdraw(src, amt);
      deposit(dst, amt);
      return Bool.True;
   }
   function withdraw (src : address, amt : uint) public {
      Assign.assign(LValueMemberAccess.memberAccess(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), src)), Num.sub(rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), src)), amt) as uint);
   }
   function deposit (dst : address, amt : uint) public {
      Assign.assign(LValueMemberAccess.memberAccess(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), dst)), Num.add(rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), dst)), amt) as uint);
   }
   function init () public {
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)), address(81985529216486895));
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), msg_sender_sel)), caller());
      Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), decimals_sel)), Num.fromWord(18));
   }
   function main () public returns (uint) {
      init();
      mint(uint(1000));
      mint(uint(1000));
      let amt = uint(1) ;
      let src : address = rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)) ;
      transferFrom(rval(MemberAccessProxy(ContractStorage(UintCxt), owner_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), msg_sender_sel)), uint(42));
      require1(Bool.True) as ();
      return rval(IndexAccessProxy(LValueMemberAccess.memberAccess(MemberAccessProxy(ContractStorage(UintCxt), balances_sel)), rval(MemberAccessProxy(ContractStorage(UintCxt), msg_sender_sel)))) as uint;
   }
}
