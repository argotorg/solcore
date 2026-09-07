pragma no-patterson-condition StorageType;
pragma no-bounded-variable-condition StorageType;

export {
    StorageDeriving,
    loadGeneric,
    storeGeneric
};

import * from std;
import {sload, sstore} from std.opcodes;
import * from std.Generic;

// Marker class. Importing this module brings StorageDeriving into scope, which
// is the signal DeriveGeneric looks for to auto-derive StorageSize / CanStore
// instances for local data types (alongside their Generic instance). It carries
// no methods — its mere visibility enables storage derivation.
trait StorageDeriving<self> {}

// ─── Storage layout for algebraic data types ─────────────────────────────
//
// This module is the storage analogue of std.ABIGeneric: it teaches the
// StorageSize / StorageType / CanStore classes how to deal with the
// primitive SOP types that `Generic` maps user data types onto
//   sum(f, g)  with constructors inl / inr   (choice / tagged union)
//   (f, g)     pair                          (product)
//   ()         unit
// and then bridges every type with a `Generic(rep)` instance to those
// layouts. `Generic` instances are auto-derived for local data types, so
// no per-type boilerplate is needed at the use site.

// ─── StorageSize for the primitive sum(f, g) type ────────────────────────
// A tagged union occupies one slot for the tag plus enough slots for the
// largest branch:  size = 1 + max(size(f), size(g)).
// (StorageSize for () and (a, b) is already provided by std.)

impl<f, g> StorageSize<sum<f, g>> where f: StorageSize, g: StorageSize {
    function size(x : Proxy<sum<f, g>>) returns (word) {
        let f_sz : word = StorageSize.size(@f);
        let g_sz : word = StorageSize.size(@g);
        return 1 + maxWord(f_sz, g_sz);
    }
}

// ─── StorageType for () ──────────────────────────────────────────────────
// The unit type occupies no slots, so load/store are no-ops.

impl StorageType<()> {
    function load(ptr : word) returns (()) {
        return;
    }
    function store(ptr : word, value : ()) returns (()) {
        return;
    }
}

// ─── StorageType for the primitive product (a, b) ────────────────────────
// Layout:  [ptr .. ptr + size(a) - 1]            : a
//          [ptr + size(a) ..             ]       : b

impl<a, b> StorageType<(a, b)> where a: StorageType, a: StorageSize, b: StorageType {
    function load(ptr : word) returns ((a, b)) {
        let a_sz : word = StorageSize.size(@a);
        let x : a = StorageType.load(ptr);
        let y : b = StorageType.load(ptr + a_sz);
        return (x, y);
    }
    function store(ptr : word, value : (a, b)) returns (()) {
        match (value ) {
        case (x, y) {
            let a_sz : word = StorageSize.size(@a);
            StorageType.store(ptr, x);
            StorageType.store(ptr + a_sz, y);
        } }
    }
}

// ─── StorageType for the primitive sum(f, g) ─────────────────────────────
// Slot layout (static sums):
//   [ptr]            : tag word (0 = inl, 1 = inr)
//   [ptr + 1 ..    ] : encoded branch payload

impl<f, g> StorageType<sum<f, g>> where f: StorageType, g: StorageType {
    function load(ptr : word) returns (sum<f, g>) {
        let tag : word = sload(ptr);
        match (tag ) {
        case 0 {
            let v : f = StorageType.load(ptr + 1);
            return inl(v);
        } default {
            let v : g = StorageType.load(ptr + 1);
            return inr(v);
        } }
    }
    function store(ptr : word, value : sum<f, g>) returns (()) {
        match (value ) {
        case inl(v) {
            sstore(ptr, 0);
            StorageType.store(ptr + 1, v);
        } case inr(v) {
            sstore(ptr, 1);
            StorageType.store(ptr + 1, v);
        } }
    }
}

// ─── Storage layout via CanStore ─────────────────────────────────────────
//
// The structural instances above teach StorageType the fixed-slot encoding of
// the SOP primitives. But StorageType can only describe word-packed types: a
// dynamically-sized field such as memory(bytes) has a StorageSize (one slot,
// Solidity-style) and a CanStore instance (storage(bytes):CanStore(memory(bytes)))
// but NO StorageType instance. Routing an ADT's storage through StorageType
// therefore rejects any data type carrying such a field, even though the field
// is perfectly storable.
//
// So we give CanStore the same structural treatment, decomposing the SOP
// representation and storing each leaf through the leaf's OWN CanStore instance.
// Fixed leaves resolve to storage(word)/storage(uint256)/… (which delegate to
// StorageType); dynamic leaves resolve to storage(bytes)/storage(string). Each
// field occupies StorageSize-many slots, so offsets are computed exactly as in
// the StorageType layout. The slot handle for a value of type `t` is uniformly
// `storage(t)`, which is why the dynamic leaves below are mirrored at that
// handle.

// The unit type occupies no slots.
impl CanStore<storage<()>, ()> {
    function store(r : storage<()>, v : ()) returns (()) {
        return;
    }
    function load(r : storage<()>) returns (()) {
        return;
    }
}

// Product: store `a` at the base slot, `b` size(a) slots later.
impl<a, b> CanStore<storage<(a, b)>, (a, b)> where storage<a>: CanStore<a>, a: StorageSize, storage<b>: CanStore<b> {
    function store(r : storage<(a, b)>, v : (a, b)) returns (()) {
        match (v ) {
        case (x, y) {
            let base : word = Typedef.rep(r);
            let a_sz : word = StorageSize.size(@a);
            let syntaxValue1: storage<a> = storage(base);
            CanStore.store(syntaxValue1, x);
            let syntaxValue2: storage<b> = storage(base + a_sz);
            CanStore.store(syntaxValue2, y);
        } }
    }
    function load(r : storage<(a, b)>) returns ((a, b)) {
        let base : word = Typedef.rep(r);
        let a_sz : word = StorageSize.size(@a);
        let syntaxValue3: storage<a> = storage(base);
        let x : a = CanStore.load(syntaxValue3);
        let syntaxValue4: storage<b> = storage(base + a_sz);
        let y : b = CanStore.load(syntaxValue4);
        return (x, y);
    }
}

// Tagged union: slot 0 holds the tag, the branch payload follows.
impl<f, g> CanStore<storage<sum<f, g>>, sum<f, g>> where storage<f>: CanStore<f>, storage<g>: CanStore<g> {
    function store(r : storage<sum<f, g>>, v : sum<f, g>) returns (()) {
        let base : word = Typedef.rep(r);
        match (v ) {
        case inl(x) {
            sstore(base, 0);
            let syntaxValue5: storage<f> = storage(base + 1);
            CanStore.store(syntaxValue5, x);
        } case inr(y) {
            sstore(base, 1);
            let syntaxValue6: storage<g> = storage(base + 1);
            CanStore.store(syntaxValue6, y);
        } }
    }
    function load(r : storage<sum<f, g>>) returns (sum<f, g>) {
        let base : word = Typedef.rep(r);
        let tag : word = sload(base);
        // NOTE: the loaded payload is inlined directly into inl(...) / inr(...)
        // rather than bound to a `let x : f` / `let y : g` first. Binding the
        // payload to an intermediate of the branch type (f or g) makes the
        // compiler infer the *branch* type for the inl/inr application instead
        // of the full sum(f, g), so it emits e.g. `inr<g>(y)` and Yul codegen
        // rejects it (sum nesting off by one). Inlining matches the working
        // ABIGeneric.decode pattern, so inl/inr pick up the full sum(f, g).
        match (tag ) {
        case 0 {
            let syntaxValue7: storage<f> = storage(base + 1);
            return inl(CanStore.load(syntaxValue7));
        } default {
            let syntaxValue8: storage<g> = storage(base + 1);
            return inr(CanStore.load(syntaxValue8));
        } }
    }
}

// Dynamic leaves at the uniform storage(t) handle. std provides the storage(bytes)
// / storage(string) instances (data lives at keccak(slot)); these mirror them at
// the storage(memory(bytes)) / storage(memory(string)) handle the structural
// decomposition asks for, so a memory(bytes) field inside an ADT is storable.
impl CanStore<storage<memory<bytes>>, memory<bytes>> {
    function store(r : storage<memory<bytes>>, v : memory<bytes>) returns (()) {
        let syntaxValue9: storage<bytes> = storage(Typedef.rep(r));
        CanStore.store(syntaxValue9, v);
    }
    function load(r : storage<memory<bytes>>) returns (memory<bytes>) {
        let syntaxValue10: storage<bytes> = storage(Typedef.rep(r));
        return CanStore.load(syntaxValue10);
    }
}

impl CanStore<storage<memory<string>>, memory<string>> {
    function store(r : storage<memory<string>>, v : memory<string>) returns (()) {
        let syntaxValue11: storage<string> = storage(Typedef.rep(r));
        CanStore.store(syntaxValue11, v);
    }
    function load(r : storage<memory<string>>) returns (memory<string>) {
        let syntaxValue12: storage<string> = storage(Typedef.rep(r));
        return CanStore.load(syntaxValue12);
    }
}

// StorageType / CanStore for an ADT are NOT provided here as blanket bridges.
//
// A `default instance a:StorageType` would have its `load` return the head
// variable `a` via Generic.to — but the specializer cannot monomorphize a
// result-position type variable of a default instance (it is not pinned by the
// arguments), so loads panic. Likewise a tyvar-headed `default a:CanStore(b)`
// is non-functional (accepts any storable b), so contract field access cannot
// infer the stored type from the slot type.
//
// Instead, DeriveGeneric emits a concrete, per-type storage(T):CanStore(T)
// instance (see Solcore.Desugarer.DeriveGeneric) where the data type is fixed
// in the instance head; it delegates to the structural CanStore instances above
// via the type's Generic representation. StorageSize is likewise derived
// per-type for the field layout.

// ─── Top-level helpers ───────────────────────────────────────────────────
// Convenience wrappers mirroring std.ABIGeneric's encode / decode: persist or
// read back any 'a' that has a Generic(rep) instance at a raw storage slot.

function storeGeneric<a, rep>(slot : word, value : a) returns (())  where a: Generic<rep>, rep: StorageType {
    StorageType.store(slot, Generic.from(value));
}

function loadGeneric<a, rep>(slot : word) returns (a)  where a: Generic<rep>, rep: StorageType {
    let r : rep = StorageType.load(slot);
    return Generic.to(r);
}
