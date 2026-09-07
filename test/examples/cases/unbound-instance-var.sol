trait C<self> {
    function size(x:self) returns (word);
}

impl C<()> {
    function size(x:()) returns (word) {
        return 0;
    }
}

impl C<uint> {
    function size(x:uint) returns (word) {
        return 1;
    }
}
