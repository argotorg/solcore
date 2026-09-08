enum Proxy<a> { Proxy }
enum dict<member, index> { dict(word, Proxy<member>, Proxy<index>) }
enum address { address(word) }
enum storage<a> { storage(word) }

function saddr<a>(s: storage<a>) returns (word) {
  match (s ) {
    case storage(a) { return a;
  } }
}


// Untyped Index (access) Proxy
enum UIP<m, idx, member> { UIP(m, idx) }
// Typed Index (access) Proxy
enum TIP<m, idx, member> { TIP(m, idx, Proxy<member>) }

function setbal(ref: storage<dict<address, word>> , src : address, amt: word) returns (()) {
  /* Based on inference:
    ref : storage(dict(address, word))
    => ref[src] : storage(word)  assuming src is of the right type
  */
  let tip = TIP(ref, src, @word);
  Assign.assign(LVA.acc(tip), amt);
}

function setAllowance(ref: storage<dict<address, dict<address, word>>>, owner : address, spender : address, amt : word) returns (()) {

  let tip1 : TIP<storage<dict<address, dict<address, word>>>, address, dict<address, word>>
           = TIP(ref, owner, @dict<address, word>);
  let ref2 : storage<dict<address, word>>  = LVA.acc(tip1);
  let tip2 : TIP<storage<dict<address, word>>, address, word>
           = TIP(ref2, spender, @word);
  let ref3 : storage<word> = LVA.acc(tip2);
  Assign.assign(ref3, amt);
}

function getAllowance(ref: storage<dict<address, dict<address, word>>>, owner : address, spender : address) returns (word) {
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
       , @dict<address, word>
       ) /* tip : TIP(storage(dict(address, dict(address, word))), address, dict(address, word)) */
      ) /* ref2 : storage(dict(address,word)) */
    , spender
    , @word
    ) /* tip2 : TIP(storage(dict(address, word)), address, word) */
  );
}

trait LVA<self, memberRefType> {
    function acc(x:self) returns (memberRefType);
}


trait RVA<self, member> {
    function acc(x:self) returns (member);
}

impl<index, member> LVA<TIP<storage<dict<index, member>>, index, member>, storage<member>> {
    function acc(x:TIP<storage<dict<index, member>>, index, member>) returns (storage<member>) {
	    return storage(42);
    }
}

impl<index, member> LVA<UIP<storage<dict<index, member>>, index, member>, storage<member>> {
    function acc(x:UIP<storage<dict<index, member>>, index, member>) returns (storage<member>) {
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

impl<index, member> RVA<TIP<storage<dict<index, member>>, index, member>, member> where member: StorageType {
    function acc(x:TIP<storage<dict<index, member>>, index, member>) returns (member) {
	    let addr = saddr(LVA.acc(x));
	    return StorageType.sload(addr);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}


impl<a> Assign<storage<a>, a> where a: StorageType {
    function assign(l:storage<a>, r:a) returns (()) {
      StorageType.store(saddr(l), r);
    }
}

contract Tiamat {
  function main() public returns (word) {
    let allowances : storage<dict<address, dict<address, word>>>;
    let src = address(17);
    setAllowance(allowances, address(1),address(2), 666);
    return getAllowance(allowances, address(1),address(2));
  }
}
