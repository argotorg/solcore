import "BasicTypes.sol"

data MemoryWordReader = MemoryWordReader(word);
data CalldataWordReader = CalldataWordReader(word, word);

class self:WordReader {
    function read(r:self) -> (self, word);
}

instance MemoryWordReader:WordReader {
    function read(r:MemoryWordReader) -> (MemoryWordReader, word) {
        match r {
            | MemoryWordReader(ptr) => {
                let result:word;
                assembly {
                    result := mload(ptr)
                    ptr := add(ptr, 32)
                }
                return MemoryWordReader(ptr);
            }
        }
    }
}

instance CalldataWordReader:WordReader {
    function read(r:CalldataWordReader) -> (CalldataWordReader, word) {
        match r {
            | CalldataWordReader(ptr, endPtr) => {
                let result:word;
                assembly {
                    result := calldataload(ptr)
                    // TODO: bounds check
                    ptr := add(ptr, 32)
                }
                return CalldataWordReader(ptr, endPtr);
            }
        }
    }
}

class self:GetWordReader(readerType) {
    function get(x:self) -> readerType;
}

instance memory(bytes):GetWordReader(MemoryWordReader) {
    function get(x:memory(bytes)) -> MemoryWordReader {
        return MemoryWordReader(Typedef.rep(x));
    }
}

// TODO: move
data calldata(word) = calldata(word);

instance calldata(bytes):GetWordReader(MemoryWordReader) {
    function get(x:calldata(bytes)) -> CalldataWordReader {
        return CalldataWordReader(Typedef.rep(x));
    }
}

forall ptr reader a . ptr:GetWordReader(reader), ABIDecoder(a,reader):ABIDecode =>
function abiDecode(x:ptr, t:Proxy(a)) -> a {
    return ABIDecode.decode(ABIDecoder(t, GetWordReader.get(ptr)));
}
