
enum memory<t> { memory(word) }

trait ValueTy<t> {
    function rep(x:t) returns (word);
}

impl<t> ValueTy<memory<t>> {
    function rep(x: memory<t>) returns (word) {
        match (x ) {
        case memory(w) { return w;
       } }
    }
}

trait Ref<ref, deref> {
    function store(loc: ref, value: deref) returns (());
}

impl<t> Ref<memory<t>, t> where t: ValueTy {
    function store(loc: memory<t>, value: t) returns (()) {
        // We don't have a `ValueTy` bound on `t` anywhere, so this should raise a type error...
        let vw = ValueTy.rep(value);
    }
}
