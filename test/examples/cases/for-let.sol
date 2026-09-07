import {Num,Add,Sub,Eq,Ord,Bounded,Typedef,le} from std;

contract Prefor {
    function main() public returns (word) {
        let s : word = 0;
        for(let i=1;i<=10;i=i+1) { s = s + i;}

        return s;
    }
}
