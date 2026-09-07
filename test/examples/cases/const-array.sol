
enum Zero {}
enum Succ<a> {}

trait TAdd<self, res> {}
impl<a> TAdd<(Zero, a), a> {}
impl<a, b, c> TAdd<(Succ<b>, a), Succ<c>> where (b, a): TAdd<c> {}

trait Eq<lhs, rhs> {}
impl<a> Eq<a, a> {}

// this should work but doesnt: forall sizel sizer elem sizeout . (sizel, sizer):TAdd(sizeout)
function concat<sizel, sizer, elem, sizeout, pairSizelSizer>(lhs:elem[sizel] memory, rhs:elem[sizer] memory) returns (elem[sizeout] memory)  where pairSizelSizer: Eq<(sizel, sizer)>, pairSizelSizer: TAdd<sizeout> {
    return memory(0) as elem[sizeout] memory; // :D
}

enum Itself<a> { ItselfRuntimeTag }

enum array<size, elem> { array }
enum memory<a> { memory(word) }

trait IndexAccessible<self, indexType, elementType> {
    function set(self:self, ix:indexType, val:elementType);
    function at(self:self, ix:indexType) returns (elementType);
}

trait ToWord<self> {
    function toWord(self:Itself<self>) returns (word);
}

impl ToWord<Zero> {
    function toWord(zero) { return 0; }
}

impl<prev> ToWord<Succ<prev>> where prev: ToWord {
    function toWord(self: Itself<Succ<prev>>) {
        let returnVal : word = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<prev>);
        assembly {
            returnVal := add(1, returnVal)
        }
        return returnVal;
    }
}

trait MemoryType<self> {
    function load(ptr:word) returns (self);
    function store(ptr:word, value:self);
}

impl MemoryType<word> {
    function load(ptr:word) returns (word) {
        let val : word;
        assembly { val := mload(ptr) }
        return val;
    }
    function store(ptr:word, value:word) {
        assembly { mstore(ptr, value) }
    }
}

impl<size, elem> IndexAccessible<elem[size] memory, word, elem> where size: ToWord, elem: MemoryType {
    function at(self, index) returns (elem) {
        let sizeValue = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<size>);
       // this should work but doesn't
        // assembly {
        //    if iszero(lt(index, sizeValue)) {
        //        revert(0, 0)
        //    }
        //}

        match (self ) {
            case memory(offset) {
                let x = offset; // can't use this inside the assembly block :-(
                assembly {
                    index := add(x, mul(32, index))
                }
                return MemoryType.load(index);
        } }
    }

    function set(self, index, val) {
        let sizeValue = ToWord.toWord(Itself.ItselfRuntimeTag as Itself<size>);

        //assembly {
        //    if iszero(lt(index, sizeValue)) {
        //        revert(0, 0)
        //    }
        //}

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

    function main() public {
        let arr : word[Succ<Succ<Succ<Succ<Zero>>>>] memory = memory(42);  // = (1,2,3,4,5,6,7,8,9,10);
        IndexAccessible.set(arr, 4, 33);

       // this (correctly) typechecks but doesn't specialize
        let res = concat(arr, arr); // this typechecks
        return IndexAccessible.at(arr, 4);
    }
}
