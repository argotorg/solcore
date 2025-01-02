/// Other used stdlib classes and types:
class self:Ref(deref) {
    function load(x:self) -> deref;
}

data Uint256 = Uint256(Word)
data Bool = True | False
data Bytes32 = Bytes32(Word)
data Unit = Unit

data Proxy(t) = Proxy
data Memory(x) = Memory(Word)

/// Specific new stdlib classes and types:

class self:StructMember(preceding, memberTy) {}
data StructMember(structType, fieldType) = StructMember

// "dead" is only here to compensate for non-relaxed coverage condition and
// incorrectly implemented Paterson condition
data MemberAccess(ty, field, dead) = MemberAccess(ty)


/// Usage Example / Proof of Concept:

/*
    struct S {
        x:Uint256;
        y:Bool;
        z:Bytes32;
    }
*/

data S = S(Pair(Uint256, Pair(Bool, Bytes32)))

data Field_x = FieldX // Selector type for "x"
data Field_y = FieldY // Selector type for "y"
data Field_z = FieldZ // Selector type for "z"

// StructMember instances for field selectors:
instance StructMember(S, Field_x):StructMember(Unit, Uint256) {}
instance StructMember(S, Field_y):StructMember(Uint256, Bool) {}
instance StructMember(S, Field_z):StructMember(Pair(Uint256, Bool), Bytes32) {}

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
class self:MemorySize {
    function memorySize(x:Proxy(self)) -> Word;
}

/// Size of the struct member types in memory:
instance Unit:MemorySize { function memorySize(x) -> Word { return 0; } }
instance Uint256:MemorySize { function memorySize(x) -> Word { return 32; } }
instance Bool:MemorySize { function memorySize(x) -> Word { return 32; } }
instance Bytes32:MemorySize { function memorySize(x) -> Word { return 32; } }

/// Memory size of pairs
instance Pair(a,b):MemorySize {
    function memorySize(x) -> Word
    {
        let pa:Proxy(a);
        let pb:Proxy(b);
        let sz = memorySize(pa);
        let szb = memorySize(pb);
        assembly { sz := add(sz, szb) }; // TODO: bounds check?
        return sz;
    }

}

/// Fragments of a generic memory implementation:
class self:MemoryType {
    function loadFromMemory(p:Proxy(self), off:Word) -> self;
}

instance Uint256:MemoryType {
    function loadFromMemory(p:Proxy(Uint256), off:Word) -> Uint256 {
        let v;
        assembly { v := mload(off) };
        return Uint256(v);
    }
}

instance (a:MemoryType) => Memory(a):Ref(a) {
    function load(x) {
        let p:Proxy(a);
        match x { | Memory(off) => return loadFromMemory(p, off); };
    }
}

/// Crucial instance: member access to struct fields in memory:

instance (
    StructMember(structType, fieldType):StructMember(precedingTuple, ty),
    precedingTuple:MemorySize,
    Memory(ty):Ref(ty)
) => MemberAccess(Memory(structType), fieldType,
    // Needs ridiculous amounts of constructor applications due to incorrect implementation of the Paterson Condition
    // Needs to mention "ty" due to non-relaxed Coverage Condition
    Memory(ty)
):Ref(ty)
{
    function load(x) {
        let ptr:Word;
        match x { | MemberAccess(Memory(y)) => ptr = y; };

        let p:Proxy(precedingTuple);
        let offset = memorySize(p);

        assembly { ptr := add(ptr, offset) };

        let tyPtr:Memory(ty) = Memory(ptr);
        return load(tyPtr);
    }
}

function test()
{
    let x:Memory(S);
    let memberAccess:MemberAccess(Memory(S), Field_x,
                                  Memory(Uint256) // will become unnecessary
    );
    memberAccess = MemberAccess(x);
    let result = load(memberAccess);
    /*
     Eventually, I imagine ``let result = x.x;`` to merely desugar to

     let result = load(MemberAccess(x):MemberAccess(_, Field_x));

     which is equivalent to the above.
     */
}

