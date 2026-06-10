import std.{Num,Add,Sub,Eq,Ord,Bounded,Typedef,le};

contract ForMultiInit {
    function main() -> word {
        let i = 0;
        let j = 0;
        for (i = 1, j = 10; i <= 3; i = i + 1) {
            j = j + i;
        }
        return j;
    }
}
