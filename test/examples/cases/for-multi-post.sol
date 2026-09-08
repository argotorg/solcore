import {Num,Add,Sub,Eq,Ord,Bounded,Typedef,le} from std;

contract ForMultiPost {
    function main() returns (word) {
        let j = 0;
        for (let i = 0; i <= 3; i = i + 1, j = j + 2) {
            j = j + i;
        }
        return j;
    }
}
