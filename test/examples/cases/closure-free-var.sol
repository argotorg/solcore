function addW (l: word, r: word) returns (word) {
     let rw : word;
     assembly {
	 rw := add(l,r)
     }
     return rw;
}

trait Add<t> {
    function add(l: t, r: t) returns (t);
}

impl Add<word> {
    function add(l: word, r: word) returns (word) { return addW(l,r); }
}

contract Bug {
    function main() public returns (word) {
        return makeClosure(42);
    }

    function makeClosure(e : word) public returns (word) {
        let f = lam (x : word) {
            return Add.add(x,e);  // this crashes
	    // return addW(e,x);  // this works
        };
        return f(1);
    }
}
