/////// Construction
trait Typedef<abs, rep> {
    function rep(x:abs) returns (rep);
    function abs(x:rep) returns (abs);
}

enum xunit { xunit }

enum uint { uint(word) }

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

enum memory<a> { memory(word) }
enum memoryRef<a> { memoryRef(word) }
enum Proxy<a> { Proxy }

impl Typedef<a memory, word> {
    function rep(x:a memory) returns (word) {
        match (x ) {
            case memory(y) { return y;
        } }
    }
    function abs(x:word) returns (a memory) {
        return memory(x);
    }
}
impl Typedef<memoryRef<a>, word> {
    function rep(x:memoryRef<a>) returns (word) {
        match (x ) {
            case memoryRef(y) { return y;
        } }
    }
    function abs(x:word) returns (memoryRef<a>) {
        return memoryRef(x);
    }
}

trait Assign<lhs, rhs> {
    function assign(l:lhs, r:rhs) returns (());
}

enum ref<a> { ref(a) }

impl Assign<ref<a>, a> {
    function assign(l:ref<a>, r:a) returns (()) {
        // builtin "stack store"
        return;
    }
}

trait MemoryType<self> {
    function load(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
}

trait MemorySize<self> {
    function size(x:Proxy<self>) returns (word);
}

impl MemoryType<word> {
    function load(ptr:word) returns (word) {
        let r:word;
        assembly {
            r := mload(ptr)
        }
        return r;
    }
    function store(ptr:word, value:word) returns (()) {
        assembly {
            mstore(ptr, value)
        }
    }
}

impl MemoryType<uint> {
    function load(ptr:word) returns (uint) {
        return Typedef.abs(MemoryType.load(ptr));
    }
    function store(ptr:word, value:uint) returns (()) {
        return MemoryType.store(ptr, Typedef.rep(value));
    }
}

impl<a> Assign<memoryRef<a>, a> where a: MemoryType {
    function assign(l:memoryRef<a>, y:a) {
        MemoryType.store(Typedef.rep(l), y);
    }
}



enum MemberAccessProxy<a, field> { MemberAccessProxy(a, Proxy<field>) }

function memberAccessPtr<a, field>(x:MemberAccessProxy<a memory, field>) returns (word) {
    match (x ) {
        case MemberAccessProxy(y,z) { match (y ) {
            case memory(ptr) { return ptr;
        } }
    } }
}

trait LValueMemberAccess<self, memberRefType> {
    function memberAccess(x:self) returns (memberRefType);
}

trait RValueMemberAccess<self, memberValueType> {
    function memberAccess(x:self) returns (memberValueType);
}

impl MemorySize<xunit> {
    function size(x:Proxy<xunit>) returns (word) {
        return 0;
    }
}

impl MemorySize<word> {
    function size(x:Proxy<word>) returns (word) {
        return 32;
    }
}


impl MemorySize<uint> {
    function size(x:Proxy<uint>) returns (word) {
        return 32;
    }
}

enum zero { zero }
enum suc<a> { suc(a) }

impl<a, b> Typedef<MemberAccessProxy<(a, b) memory, zero>, word> {}
impl<a, b> LValueMemberAccess<MemberAccessProxy<(a, b) memory, zero>, memoryRef<a>> {
    function memberAccess(mptr:MemberAccessProxy<(a, b) memory, zero>, f:Proxy<zero>) returns (memoryRef<a>) {
        let ptr:word = Typedef.rep(mptr);
        return memoryRef(ptr);
    }
}

impl<a, b, c, n> LValueMemberAccess<MemberAccessProxy<(a, b) memory, suc<n>>, c> where MemberAccessProxy<b memory, n>: LValueMemberAccess<c>, a: MemorySize {
    function memberAccess(map:MemberAccessProxy<(a, b) memory, suc<n>>, f:Proxy<suc<n>>) returns (c) {
        let ptr:word = memberAccessPtr(map);
        let sz:word = MemorySize.size(Proxy as Proxy<a>);
        assembly { ptr := add(ptr, sz) }
        let newPtr:b memory = memory(ptr);
        return LValueMemberAccess.memberAccess(MemberAccessProxy(newPtr, Proxy as Proxy<n>));
    }
}

impl LValueMemberAccess<MemberAccessProxy<a memory, zero>, word> {}
impl LValueMemberAccess<MemberAccessProxy<a memory, suc<zero>>, uint> {}
impl LValueMemberAccess<MemberAccessProxy<a memory, suc<suc<zero>>>, word> {}
impl Assign<word, word> {}
impl Assign<uint, uint> {}

////// Testing

// struct S { x:word; y:uint; z:word; }
enum S { S(word, uint, word) }
enum x_sel { x_sel }
enum y_sel { y_sel }
enum z_sel { z_sel }

impl Typedef<S, (word, uint, word)> {
    function abs(x:(word, uint, word)) returns (S) {
        match (x ) {
            case (a, b, c) { return S(a, b, c);
        } }
    }
    function rep(x:S) returns ((word, uint, word)) {
        match (x ) {
            case S(a, b, c) { return (a, b, c);
        } }
    }
}


// The idea here would be to generate these particularly on the definition of a struct with fields.
impl<c, rep> LValueMemberAccess<MemberAccessProxy<S memory, x_sel>, word> where S: Typedef<rep>, MemberAccessProxy<rep memory, zero>: LValueMemberAccess<word> {
    function memberAccess(map:MemberAccessProxy<S memory, x_sel>, f:Proxy<x_sel>) returns (word) {
        return (LValueMemberAccess.memberAccess(MemberAccessProxy(memory(memberAccessPtr(map)) as rep memory, Proxy as Proxy<zero>)) as word);
    }
}

impl<c, rep> LValueMemberAccess<MemberAccessProxy<S memory, y_sel>, uint> where S: Typedef<rep>, MemberAccessProxy<rep memory, suc<zero>>: LValueMemberAccess<uint> {
    function memberAccess(map:MemberAccessProxy<S memory, y_sel>, f:Proxy<y_sel>) returns (uint) {
        return LValueMemberAccess.memberAccess(MemberAccessProxy(memory(memberAccessPtr(map)) as rep memory, Proxy as Proxy<suc<zero>>));
    }
}

impl<c, rep> LValueMemberAccess<MemberAccessProxy<S memory, z_sel>, word> where S: Typedef<rep>, MemberAccessProxy<rep memory, suc<suc<zero>>>: LValueMemberAccess<word> {
    function memberAccess(map:MemberAccessProxy<S memory, z_sel>, f:Proxy<z_sel>) returns (word) {
        return LValueMemberAccess.memberAccess(MemberAccessProxy(memory(memberAccessPtr(map)) as rep memory, Proxy as Proxy<suc<suc<zero>>>));
    }
}

function f() {
    let x:word memory;
    let y:word memory;
    x = y;
}

function g() {
    let s:S memory = Typedef.abs(0x80);
    let x:word = 42;
    let y:uint = Typedef.abs(21);
    let z:word = 7;
    // s.x = x;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, Proxy as Proxy<x_sel>)), x);
    // s.y = y;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, Proxy as Proxy<y_sel>)), y);
    // s.z = z;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, Proxy as Proxy<z_sel>)), z);
}

contract C {
    function main() public {
        f();
        g();
    }
}
