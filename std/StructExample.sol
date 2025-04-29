import "MemberAccess.sol"
import "BasicTypes.sol"


/*

struct S {
    x:uint;
    y:word;
    z:bool;
}

 */

data S = S(uint, word, bool);

instance S:Typedef((uint, word, bool)) {
    function rep(x:S) -> (uint, word, bool) {
        match x {
            | S(a,b,c) => return (a,b,c);
        }
    }
    function abs(x:(uint, word, bool)) -> S {
        match x {
            | (a,b,c) => S(a,b,c);
        }
    }
}

data x_sel = x_sel;
data y_sel = y_sel;
data z_sel = z_sel;

instance MemberAccessProxy(S, x_rel):RepField((uint, word, bool), zero) {}
instance MemberAccessProxy(S, y_rel):RepField((uint, word, bool), suc(zero)) {}
instance MemberAccessProxy(S, z_rel):RepField((uint, word, bool), suc(suc(zero))) {}

contract C {
    function main() {

    }
}
