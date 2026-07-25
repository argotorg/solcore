enum storage<a> { storage(word) }
enum storageRef<a> { storageRef(word) }
enum Proxy<a> { Proxy }

enum mapping<member, index> { mapping(word, Proxy<member>, Proxy<index>) } // storage by default
// data mapRef(a) = mapRef(word); //ref to a map elem

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}

impl<a> Assign<storageRef<a>, a> {
    function assign(l:storageRef<a>, y:a) returns (()) {

    }
}

trait CStructField<self, fieldType, offsetType> {}
enum StructField<structType, fieldSelector> { StructField(structType) }


enum MemberAccessProxy<a, field, offset> { MemberAccessProxy(a, field) }


trait LValueMemberAccess<self, memberRefType> {
    function memberAccess(x:self) returns (memberRefType);
}

// ------------------------------------------------------------------
// Contract field access
// ------------------------------------------------------------------


impl<cxt, fieldSelector, fieldType, offsetType> LValueMemberAccess<MemberAccessProxy<cxt, fieldSelector, offsetType>, storageRef<fieldType>> where StructField<cxt, fieldSelector>: CStructField<fieldType, offsetType> {
    function memberAccess(x:MemberAccessProxy<cxt, fieldSelector, offsetType>) returns (storageRef<fieldType>) {
        return storageRef(0x100);
    }
}

// ------------------------------------------------------------------
// Indexed access
// ------------------------------------------------------------------

enum mapping<index, member> { mapping(word) }
enum IndexAccessProxy<map, index, member> { IndexAccessProxy(map, index) }
enum IndexAccessProxy2<map, index, member> { IndexAccessProxy2(map, index, Proxy<member>) }

impl<map, index, member> LValueMemberAccess<IndexAccessProxy<storageRef<mapping(index => member)>, index, member>, storageRef<member>> {
    function memberAccess(x:IndexAccessProxy<storageRef<map>, index, member>) returns (storageRef<member>) {
	    return storageRef(0);
    }
}

enum MintCtx { MintCtx }
enum balances_sel { balances_sel }
impl CStructField<StructField<MintCtx, balances_sel>, mapping(word => word), ()> {}

   function mint(amount:word) {
     let bal_prx = MemberAccessProxy(MintCtx, balances_sel);
     let bal_ref = LValueMemberAccess.memberAccess(bal_prx);

     Assign.assign(
       LValueMemberAccess.memberAccess(
         IndexAccessProxy(
	 // bal_ref // this works, but inlining bal_ref leads to error
	 LValueMemberAccess.memberAccess(bal_prx)
	 , 0
	 )
       )
     , amount
     ) ;

   }
