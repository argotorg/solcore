pragma solcore noCoverageCondition TAdd;

enum Zero {}
enum Succ<a> {}

trait TAdd<self, res> {}
impl<a> TAdd<(Zero, a), a> {}
impl<a, b, c> TAdd<(Succ<b>, a), Succ<c>> where (b, a): TAdd<c> {}

trait Eq<lhs, rhs> {}
impl<a> Eq<a, a> {}

// this should work but doesnt: forall sizel sizer elem sizeout . (sizel, sizer):TAdd(sizeout)
// TODO: this panics during specialization
/*
forall sizel sizer elem sizeout pairSizelSizer . pairSizelSizer:Eq((sizel, sizer)), pairSizelSizer:TAdd(sizeout) => function concat(lhs:memory(array(sizel, elem)), rhs:memory(array(sizer, elem))) -> memory(array(sizeout, elem)) {
    return memory(0) : memory(array(sizeout, elem)); // :D
}
*/
enum Itself<a> { ItselfRuntimeTag }

enum array<size, elem> { array }
enum memory<a> { memory(word) }

trait IndexAccessible<self, indexType, elementType> {
    function set(self:self, ix:indexType, val:elementType) returns (());
    function at(self:self, ix:indexType) returns (elementType);
}

trait ToWord<self> {
    function toWord(self:Itself<self>) returns (word);
}

impl ToWord<Zero> {
    function toWord(zero : Itself<Zero>) returns (word) { return 0; }
}

impl<prev> ToWord<Succ<prev>> where prev: ToWord {
    function toWord(self: Itself<Succ<prev>>) returns (word) {
        let returnVal : word = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<prev>);
        assembly {
            returnVal := add(1, returnVal)
        }
        return returnVal;
    }
}

trait MemoryType<self> {
    function load(ptr:word) returns (self);
    function store(ptr:word, value:self) returns (());
}

impl MemoryType<word> {
    function load(ptr:word) returns (word) {
        let val : word;
        assembly { val := mload(ptr) }
        return val;
    }
    function store(ptr:word, value:word) returns (()) {
        assembly { mstore(ptr, value) }
    }
}

impl<size, elem> IndexAccessible<elem[size] memory, word, elem> where size: ToWord, elem: MemoryType {
    function at(self : elem[size] memory, index : word) returns (elem) {
        let sizeValue = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<size>);

        assembly {
            if iszero(lt(index, sizeValue)) {
                revert(0, 0)
            }
        }

        match (self ) {
            case memory(offset) {
                let x = offset; // can't use this inside the assembly block :-(
                assembly {
                    index := add(x, mul(32, index))
                }
                return MemoryType.load(index);
        } }
    }

    function set(self : elem[size] memory, index : word, val : elem) returns (()) {
        let sizeValue = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<size>);

        assembly {
            if iszero(lt(index, sizeValue)) {
                revert(0, 0)
            }
        }

        match (self ) {
            case memory(offset) {
            let x = offset; // can't use this inside the assembly block :-(
                assembly {
                    index := add(x, mul(32, index))
                }
                MemoryType.store(index, val);
        } }
    }
}



contract Array {

    function main() public returns (word) {
        let arr : word[Succ<Succ<Succ<Succ<Zero>>>>] memory = memory(42);  // = (1,2,3,4,5,6,7,8,9,10);
        IndexAccessible.set(arr, 3, 33);

        return IndexAccessible.at(arr, 3);
    }
}
