enum Proxy<a> { Proxy }
enum dict<member, index> { dict(word, Proxy<member>, Proxy<index>) }
enum address { address(word) }
enum storage<a> { storage(word) }

enum IndexAP<m, idx, member> { IndexAP(m, idx, Proxy<member>) }

function wal(ref: dict<address, word> storage , src : address, amt: word) returns (()) {
  let ip = IndexAP(ref, src, Proxy as Proxy<word>);
  Assign.assign(LVA.acc(ip), amt);
}


/* Expected:

ip : IndexAP(storage(dict(address, word)) , address, ?1)

LVA.acc : forall self memberRefType. self:LVA(memberRefType) => self -> memberRefType

instance IndexAP(storage(dict(index,member)), index, member):LVA(storage(member))

 |- instance IndexAP(storage(map(address, word)), address, ?1) : LVA(storage(word))) where ?1 ~ word

*/

/* Actual

> Enter reduce() |- (?l4 : Assign (word), IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4))
> Reducing wanted constraints:(?l4 : Assign (word), IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4)) using ()
> After entailment:(?l4 : Assign (word), IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4)) - ()
>> Before eliminating equalities (?l4 : Assign (word), IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4))
>> After eliminating equalities:(IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4), ?l4 : Assign (word))
>>> Found instance for:IndexAP(storage(dict(address, word)), address, ?e4) : LVA (?l4)

>>>Instance:?a5 ~ storage(?b5) => IndexAP(storage(dict(?c5, ?b5)), ?c5, ?b5) : LVA (?a5) !!!
>>>Subst:{?c5 +-> address, ?b5 +-> word, ?b5 +-> ?e4, ?l4 +-> ?a5}  ???

b5 +-> e4 should really be b5 ~ e4

*/
trait LVA<self, memberRefType> {
    function acc(x:self) returns (memberRefType);
}

impl<index, member> LVA<IndexAP<dict<index, member> storage, index, member>, member storage> {
    function acc(x:IndexAP<dict<index, member> storage, index, member>) returns (member storage) {
	    return storage(30);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}

impl<a> Assign<a storage, a> {
    function assign(l:a storage, y:a) returns (()) {}
}
