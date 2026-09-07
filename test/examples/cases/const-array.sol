
enum Zero {}
enum Succ<a> {}

trait TAdd<self, res> {}
impl<a> TAdd<(Zero, a), a> {}
impl<a, b, c> TAdd<(Succ<b>, a), Succ<c>> where (b, a): TAdd<c> {}

trait Eq<lhs, rhs> {}
impl<a> Eq<a, a> {}

// this should work but doesnt: forall sizel sizer elem sizeout . (sizel, sizer):TAdd(sizeout)
function concat<sizel, sizer, elem, sizeout, pairSizelSizer>(lhs:memory<array<sizel, elem>>, rhs:memory<array<sizer, elem>>) returns (memory<array<sizeout, elem>>)  where pairSizelSizer: Eq<(sizel, sizer)>, pairSizelSizer: TAdd<sizeout> {
    let syntaxValue1: memory<array<sizeout, elem>> = memory(0);
    return syntaxValue1; // :D
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
        let syntaxValue2: Itself<prev> = Itself.ItselfRuntimeTag;
        let returnVal : word = ToWord.toWord(syntaxValue2);
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

impl<size, elem> IndexAccessible<memory<array<size, elem>>, word, elem> where size: ToWord, elem: MemoryType {
    function at(self, index) returns (elem) {
        let syntaxValue3: Itself<size> = Itself.ItselfRuntimeTag;
        let sizeValue = ToWord.toWord(syntaxValue3);
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
        let syntaxValue4: Itself<size> = Itself.ItselfRuntimeTag;
        let sizeValue = ToWord.toWord(syntaxValue4);

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
        let arr : memory<array<Succ<Succ<Succ<Succ<Zero>>>>, word>> = memory(42);  // = (1,2,3,4,5,6,7,8,9,10);
        IndexAccessible.set(arr, 4, 33);

       // this (correctly) typechecks but doesn't specialize
        let res = concat(arr, arr); // this typechecks
        return IndexAccessible.at(arr, 4);
    }
}
