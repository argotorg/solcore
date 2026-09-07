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

impl Typedef<memory<a>, word> {
    function rep(x:memory<a>) returns (word) {
        match (x ) {
            case memory(y) { return y;
        } }
    }
    function abs(x:word) returns (memory<a>) {
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

function memberAccessPtr<a, field>(x:MemberAccessProxy<memory<a>, field>) returns (word) {
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

impl<a, b> Typedef<MemberAccessProxy<memory<(a, b)>, zero>, word> {}
impl<a, b> LValueMemberAccess<MemberAccessProxy<memory<(a, b)>, zero>, memoryRef<a>> {
    function memberAccess(mptr:MemberAccessProxy<memory<(a, b)>, zero>, f:Proxy<zero>) returns (memoryRef<a>) {
        let ptr:word = Typedef.rep(mptr);
        return memoryRef(ptr);
    }
}

impl<a, b, c, n> LValueMemberAccess<MemberAccessProxy<memory<(a, b)>, suc<n>>, c> where MemberAccessProxy<memory<b>, n>: LValueMemberAccess<c>, a: MemorySize {
    function memberAccess(map:MemberAccessProxy<memory<(a, b)>, suc<n>>, f:Proxy<suc<n>>) returns (c) {
        let ptr:word = memberAccessPtr(map);
        let sz:word = MemorySize.size(@a);
        assembly { ptr := add(ptr, sz) }
        let newPtr:memory<b> = memory(ptr);
        return LValueMemberAccess.memberAccess(MemberAccessProxy(newPtr, @n));
    }
}

impl LValueMemberAccess<MemberAccessProxy<memory<a>, zero>, word> {}
impl LValueMemberAccess<MemberAccessProxy<memory<a>, suc<zero>>, uint> {}
impl LValueMemberAccess<MemberAccessProxy<memory<a>, suc<suc<zero>>>, word> {}
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
impl<c, rep> LValueMemberAccess<MemberAccessProxy<memory<S>, x_sel>, word> where S: Typedef<rep>, MemberAccessProxy<memory<rep>, zero>: LValueMemberAccess<word> {
    function memberAccess(map:MemberAccessProxy<memory<S>, x_sel>, f:Proxy<x_sel>) returns (word) {
        let syntaxValue4: memory<rep> = memory(memberAccessPtr(map));
        let syntaxValue3: word = LValueMemberAccess.memberAccess(MemberAccessProxy(syntaxValue4, @zero));
        return (syntaxValue3);
    }
}

impl<c, rep> LValueMemberAccess<MemberAccessProxy<memory<S>, y_sel>, uint> where S: Typedef<rep>, MemberAccessProxy<memory<rep>, suc<zero>>: LValueMemberAccess<uint> {
    function memberAccess(map:MemberAccessProxy<memory<S>, y_sel>, f:Proxy<y_sel>) returns (uint) {
        let syntaxValue1: memory<rep> = memory(memberAccessPtr(map));
        return LValueMemberAccess.memberAccess(MemberAccessProxy(syntaxValue1, @suc<zero>));
    }
}

impl<c, rep> LValueMemberAccess<MemberAccessProxy<memory<S>, z_sel>, word> where S: Typedef<rep>, MemberAccessProxy<memory<rep>, suc<suc<zero>>>: LValueMemberAccess<word> {
    function memberAccess(map:MemberAccessProxy<memory<S>, z_sel>, f:Proxy<z_sel>) returns (word) {
        let syntaxValue2: memory<rep> = memory(memberAccessPtr(map));
        return LValueMemberAccess.memberAccess(MemberAccessProxy(syntaxValue2, @suc<suc<zero>>));
    }
}

function f() {
    let x:memory<word>;
    let y:memory<word>;
    x = y;
}

function g() {
    let s:memory<S> = Typedef.abs(0x80);
    let x:word = 42;
    let y:uint = Typedef.abs(21);
    let z:word = 7;
    // s.x = x;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, @x_sel)), x);
    // s.y = y;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, @y_sel)), y);
    // s.z = z;
    Assign.assign(LValueMemberAccess.memberAccess(MemberAccessProxy(s, @z_sel)), z);
}

contract C {
    function main() public {
        f();
        g();
    }
}
