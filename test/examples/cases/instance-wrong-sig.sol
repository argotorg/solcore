enum uint256 { uint256(word) }
enum Proxy<a> { Proxy }
trait ABIAttribs<self> {
    function headSize(ty:Proxy<self>) returns (word);
    function isStatic(ty:Proxy<self>) returns (bool);
}

impl ABIAttribs<()> {
    function headSize(ty : Proxy<uint256>) returns (word) { return 0; }
    function isStatic(ty : Proxy<uint256>) returns (bool) { return true; }
}
impl ABIAttribs<uint256> {
    function headSize(ty : Proxy<uint256>) returns (word) { return 32; }
    function isStatic(ty : Proxy<uint256>) returns (bool) { return true; }
}
