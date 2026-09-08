enum bytes32 { bytes32(word) }

trait Memory<t> {
    function encodeInto(v: t, target: word);
}

impl Memory<bytes32> {
    function encodeInto(v: bytes32, target: word) {}
}
