enum Unit { Unit }
enum Pair<a, b> { Pair(a, b) }

type uint = word;
type string = word;
type bool = word;

enum Memory<t> { Memory(Word) }

// this lets us link a given field in a struct to its position in it's
// underlying generic representation as a tuple.
trait Field<self, prevTypes, ty> {}

// this struct should desugar into the following
//struct S {
//  f1 : uint;
//  f2 : string;
//  f3 : bool;
//}

// a type abstraction over tuples
type s = Pair<uint, Pair<string, bool>>;

// unique types identifying each field
type sf1 = Unit;
type sf2 = Unit;
type sf3 = Unit;

// Field instances linking each field to it's position in the underlying tuple
impl Field<Pair<s, sf1>, Unit, uint> {}
impl Field<Pair<s, sf2>, uint, string> {}
impl Field<Pair<s, sf3>, Pair<uint, string>, bool> {}


// struct field member access desugars into calls to this class
trait HasField<self, fieldType> {
  function getField(x:self) returns (fieldType);
}

// we instantiate generic instances for references to types that implement Field
impl HasField<Pair<Memory<t>, fieldName>, Memory<fieldType>> where Pair<t, fieldName>: Field<prevTypes, fieldType>, fieldType: ValueType {
  function getField(x : Pair<Memory<T>, fieldName>) returns (fieldType) {
    // TODO: define this function...
    let x : Proxy<prevTypes> = Proxy;
    let sz : Word = getMemorySize(x);
    let ret : fieldType = ValueType.abs(0);
    assembly {
      ret := mload(add(rep(fst(x)), sz))
    }
    return ret;
  }
}
