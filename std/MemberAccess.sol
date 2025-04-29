import "Memory.sol";

class expr:MemberAccess(fieldType) {
    function access(x:expr) -> fieldType;
}

data MemberAccessProxy(expr, fieldSel) = MemberAccessProxy(expr, Proxy(fieldSel));

forall expr fieldSel .
instance MemberAccessProxy(expr, fieldSel):Typedef(expr) {
    function rep(x:MemberAccessProxy(expr, fieldSel)) -> expr {
        match x {
            | MemberAccessProxy(y, f) => return y;
        }
    }
    function abs(x:expr) -> MemberAccessProxy(expr, fieldSel) {
        return MemberAccessProxy(x, Proxy:Proxy(fieldSel));
    }
}

class expr:MemoryFieldOffset {
    function offset(x:Proxy(expr)) -> word;
}

class self:RepField(rep, field) {}

forall abs rep fieldSel repField . memberAccessProxyType:RepField(rep, repField), MemberAccessProxy(rep, repField):MemoryFieldOffset =>
default instance memberAccessProxyType:MemoryFieldOffset {
    function offset(x:Proxy(memberAccessProxyType)) -> word {
        return MemoryFieldOffset.offset(Proxy:Proxy(MemberAccessProxy(rep, repField)));
    }
}

instance MemberAccessProxy(a, zero):MemoryFieldOffset {
    function offset(x:_) -> word {
        return 0;
    }
}

forall a . a:MemorySize, MemberAccessProxy(b,n):MemoryFieldOffset =>
instance MemberAccessProxy((a,b), suc(n)):MemoryFieldOffset {
    function offset(x:_) -> word {
        let r:word = MemorySize.size(Proxy:Proxy(a));
        let tailOffset = MemoryFieldOffset.offset(Proxy:Proxy(MemberAccessProxy(b,n)));
        assembly { r := add(r, tailOffset) }
        return r;
    }
}

forall expr fieldSel . MemberAccessProxy(expr, fieldSel):MemoryFieldOffset =>
/* compeval */ function memoryFieldOffset(e:Proxy(expr), f:Proxy(fieldSel)) -> word {
    return MemoryFieldOffset.offset(Proxy:Proxy(MemberAccessProxy(expr, fieldSel)));

}

instance MemberAccessProxy(memory(a), fieldSel):MemberAccess(fieldType) {
    function access(expr:MemberAccessProxy(memory(a), fieldSel)) -> fieldType {
        let ptr = Typedef.rep(Typedef.rep(expr));
        let offset = /* compeval */ memoryFieldOffset(Proxy:Proxy(a), Proxy:Proxy(fieldSel));
        assembly { ptr := add(ptr, offset) }
        return MemoryType.load(ptr);
    }

}

instance MemberAccessProxy(ref(memory(a)), fieldSel):MemberAccess(memory(fieldType)) {
    function access(expr:MemberAccessProxy(ref(memory(a)), fieldSel)) -> memory(fieldType) {
        let ptr = Typedef.rep(Typedef.rep(Typedef.rep(expr)));
        let offset = /* compeval */ memoryFieldOffset(Proxy:Proxy(a), Proxy:Proxy(fieldSel));
        assembly { ptr := add(ptr, offset) }
        return memory(ptr);
    }
}
