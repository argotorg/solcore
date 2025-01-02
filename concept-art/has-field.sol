data Unit = Unit
data Pair(a, b) = Pair(a,b)

type uint = word
type string = word
type bool = word

data Memory(t) = Memory(Word)

// this lets us link a given field in a struct to its position in it's
// underlying generic representation as a tuple. 
class self:Field(prevTypes, ty) {}

// this struct should desugar into the following
//struct S {
//  f1 : uint;
//  f2 : string;
//  f3 : bool;
//}

// a type abstraction over tuples
type s = Pair(uint, Pair(string, bool))

// unique types identifying each field
type sf1 = Unit
type sf2 = Unit
type sf3 = Unit

// Field instances linking each field to it's position in the underlying tuple
instance Pair(s, sf1):Field(Unit, uint) {}
instance Pair(s, sf2):Field(uint, string) {}
instance Pair(s, sf3):Field(Pair(uint, string), bool) {}


// struct field member access desugars into calls to this class
class self:HasField(fieldType) {
  function getField(x:self) -> fieldType;
}

// we instantiate generic instances for references to types that implement Field
instance (Pair(t, fieldName):Field(prevTypes, fieldType), fieldType:ValueType) => Pair(Memory(t), fieldName):HasField(Memory(fieldType)) {
  function getField(x : Pair(Memory(T), fieldName)) -> fieldType {
    // TODO: define this function...
    let x : Proxy(prevTypes) = Proxy;
    let sz : Word = getMemorySize(x);
    let ret : fieldType = ValueType.abs(0);
    assembly {
      ret := mload(add(rep(fst(x)), sz))
    };
    return ret;
  }
}
