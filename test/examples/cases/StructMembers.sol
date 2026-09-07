/// Other used stdlib classes and types:
trait Ref<self, deref> {
    function load(x:self) returns (deref);
}

enum Uint256 { Uint256(Word) }
enum Bool { True, False }
enum Bytes32 { Bytes32(Word) }
enum Unit { Unit }

enum Proxy<t> { Proxy }
enum Memory<x> { Memory(Word) }

/// Specific new stdlib classes and types:

trait StructMember<self, preceding, memberTy> {}
enum StructMember<structType, fieldType> { StructMember }

// "dead" is only here to compensate for non-relaxed coverage condition and
// incorrectly implemented Paterson condition
enum MemberAccess<ty, field, dead> { MemberAccess(ty) }


/// Usage Example / Proof of Concept:

/*
    struct S {
        x:Uint256;
        y:Bool;
        z:Bytes32;
    }
*/

enum S { S(Pair<Uint256, Pair<Bool, Bytes32>>) }

enum Field_x { FieldX } // Selector type for "x"
enum Field_y { FieldY } // Selector type for "y"
enum Field_z { FieldZ } // Selector type for "z"

// StructMember instances for field selectors:
impl StructMember<StructMember<S, Field_x>, Unit, Uint256> {}
impl StructMember<StructMember<S, Field_y>, Uint256, Bool> {}
impl StructMember<StructMember<S, Field_z>, Pair<Uint256, Bool>, Bytes32> {}

/* Further compiler-internal builtin instances for use on stack (at least the stackref versions cannot be expressed in-language,
 * but none of these rely on any layout other than the compiler-builtin stack layout, so we can handle these purely internally
 * as "compiler magic"):
 */
/*
 instance MemberAccess(S, Field_x):Ref(Uint256);
 instance MemberAccess(stackref(S), Field_x):Ref(stackref(Uint256));
 instance MemberAccess(S, Field_y):Ref(Bool);
 instance MemberAccess(stackref(S), Field_y):Ref(stackref(Bool));
 instance MemberAccess(S, Field_z):Ref(Bytes32);
 instance MemberAccess(stackref(S), Field_z):Ref(stackref(Bytes32));
*/


/// Size of a type in memory
trait MemorySize<self> {
    function memorySize(x:Proxy<self>) returns (Word);
}

/// Size of the struct member types in memory:
impl MemorySize<Unit> { function memorySize(x : Proxy<Unit>) returns (Word) { return 0; } }
impl MemorySize<Uint256> { function memorySize(x : Proxy<Uint256>) returns (Word) { return 32; } }
impl MemorySize<Bool> { function memorySize(x : Proxy<Bool>) returns (Word) { return 32; } }
impl MemorySize<Bytes32> { function memorySize(x : Proxy<Bytes32>) returns (Word) { return 32; } }

/// Memory size of pairs
impl MemorySize<Pair<a, b>> {
    function memorySize(x : Proxy<(a, b)>) returns (Word)
    {
        let pa:Proxy<a>;
        let pb:Proxy<b>;
        let sz = memorySize(pa);
        let szb = memorySize(pb);
        assembly { sz := add(sz, szb) } // TODO: bounds check?
        return sz;
    }

}

/// Fragments of a generic memory implementation:
trait MemoryType<self> {
    function loadFromMemory(p:Proxy<self>, off:Word) returns (self);
}

impl MemoryType<Uint256> {
    function loadFromMemory(p:Proxy<Uint256>, off:Word) returns (Uint256) {
        let v;
        assembly { v := mload(off) }
        return Uint256(v);
    }
}

impl Ref<Memory<a>, a> where a: MemoryType {
    function load(x : Memory<a>) returns (a) {
        let p:Proxy<a>;
        match (x ) { case Memory(off) { return loadFromMemory(p, off); } }
    }
}

/// Crucial instance: member access to struct fields in memory:

// Needs ridiculous amounts of constructor applications due to incorrect implementation of the Paterson Condition
// Needs to mention "ty" due to non-relaxed Coverage Condition
impl Ref<MemberAccess<Memory<structType>, fieldType, Memory<ty>>, ty> where StructMember<structType, fieldType>: StructMember<precedingTuple, ty>, precedingTuple: MemorySize, Memory<ty>: Ref<ty> {
    function load(x : MemberAccess<Memory<structType>, fieldType, Memory<ty>>) returns (ty) {
        let ptr:Word;
        match (x ) { case MemberAccess(Memory(y)) { ptr = y; } }

        let p:Proxy<precedingTuple>;
        let offset = memorySize(p);

        assembly { ptr := add(ptr, offset) }

        let tyPtr:Memory<ty> = Memory(ptr);
        return load(tyPtr);
    }
}

function test() returns (())
{
    let x:Memory<S>;
    let memberAccess:MemberAccess<Memory<S>, Field_x, Memory<Uint256>>;
    memberAccess = MemberAccess(x);
    let result = load(memberAccess);
    /*
     Eventually, I imagine ``let result = x.x;`` to merely desugar to

     let result = load(MemberAccess(x):MemberAccess(_, Field_x));

     which is equivalent to the above.
     */
}

