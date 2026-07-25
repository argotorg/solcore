enum Proxy<a> { Proxy }
enum dict<member, index> { dict(word, Proxy<member>, Proxy<index>) }
enum address { address(word) }
enum storage<a> { storage(word) }

function saddr<a>(s: a storage) returns (word) {
  match (s ) {
    case storage(a) { return a;
  } }
}


// Untyped Index (access) Proxy
enum UIP<m, idx, member> { UIP(m, idx) }
// Typed Index (access) Proxy
enum TIP<m, idx, member> { TIP(m, idx, Proxy<member>) }

function setbal(ref: dict<address, word> storage , src : address, amt: word) returns (()) {
  /* Based on inference:
    ref : storage(dict(address, word))
    => ref[src] : storage(word)  assuming src is of the right type
  */
  let tip = TIP(ref, src, Proxy as Proxy<word>);
  Assign.assign(LVA.acc(tip), amt);
}

function setAllowance(ref: dict<address, dict<address, word>> storage, owner : address, spender : address, amt : word) returns (()) {

  let tip1 : TIP<dict<address, dict<address, word>> storage, address, dict<address, word>>
           = TIP(ref, owner, Proxy as Proxy<dict<address, word>>);
  let ref2 : dict<address, word> storage  = LVA.acc(tip1);
  let tip2 : TIP<dict<address, word> storage, address, word>
           = TIP(ref2, spender, Proxy as Proxy<word>);
  let ref3 : word storage = LVA.acc(tip2);
  Assign.assign(ref3, amt);
}

function getAllowance(ref: dict<address, dict<address, word>> storage, owner : address, spender : address) returns (word) {
/*
  let tip : TIP(storage(dict(address, dict(address, word))), address, dict(address, word))
           = TIP(ref, owner, Proxy:Proxy(dict(address, word)  ));
  let ref2 : storage(dict(address,word))  = LVA.acc(tip);
  let tip2 : TIP(storage(dict(address, word)), address, word)
           = TIP(ref2, spender, Proxy:Proxy(word));
*/
  return RVA.acc(
    TIP
    ( LVA.acc(
       TIP
       (ref
       , owner
       , Proxy as Proxy<dict<address, word>>
       ) /* tip : TIP(storage(dict(address, dict(address, word))), address, dict(address, word)) */
      ) /* ref2 : storage(dict(address,word)) */
    , spender
    , Proxy as Proxy<word>
    ) /* tip2 : TIP(storage(dict(address, word)), address, word) */
  );
}

trait LVA<self, memberRefType> {
    function acc(x:self) returns (memberRefType);
}


trait RVA<self, member> {
    function acc(x:self) returns (member);
}

impl<index, member> LVA<TIP<dict<index, member> storage, index, member>, member storage> {
    function acc(x:TIP<dict<index, member> storage, index, member>) returns (member storage) {
	    return storage(42);
    }
}

impl<index, member> LVA<UIP<dict<index, member> storage, index, member>, member storage> {
    function acc(x:UIP<dict<index, member> storage, index, member>) returns (member storage) {
	    return storage(42);
    }
}

trait StorageType<self> {
    function sload(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
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

impl<index, member> RVA<TIP<dict<index, member> storage, index, member>, member> where member: StorageType {
    function acc(x:TIP<dict<index, member> storage, index, member>) returns (member) {
	    let addr = saddr(LVA.acc(x));
	    return StorageType.sload(addr);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}


impl<a> Assign<a storage, a> where a: StorageType {
    function assign(l:a storage, r:a) returns (()) {
      StorageType.store(saddr(l), r);
    }
}

contract Tiamat {
  function main() public returns (word) {
    let allowances : dict<address, dict<address, word>> storage;
    let src = address(17);
    setAllowance(allowances, address(1),address(2), 666);
    return getAllowance(allowances, address(1),address(2));
  }
}
