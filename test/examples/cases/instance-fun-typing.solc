data uint256 = uint256(word);
data Proxy(a) = Proxy;
forall self . class self:ABIAttribs {
    function headSize(ty:Proxy(self)) -> word;
    function isStatic(ty:Proxy(self)) -> bool;
}

instance ():ABIAttribs {
    function headSize(ty : Proxy(uint256)) -> word { return 0; }
    function isStatic(ty : Proxy(uint256)) -> bool { return true; }
}
instance uint256:ABIAttribs {
    function headSize(ty : Proxy(uint256)) -> word { return 32; }
    function isStatic(ty : Proxy(uint256)) -> bool { return true; }
}
