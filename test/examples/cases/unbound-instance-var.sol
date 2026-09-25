trait C<self> {
    function size(x:self) returns (word);
}

impl C<unit> {
    function size(x:unit) returns (word) {
        return 0;
    }
}

impl C<uint> {
    function size(x:uint) returns (word) {
        return 1;
    }
}
