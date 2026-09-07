enum TagA { TagA(word) }
enum TagB { TagB(word) }

trait Tag<self, rep> {
    function getTag(x:self) returns (rep);
}

enum TypeA { TypeA(word) }
impl Tag<TypeA, TagA> {
    function getTag(x:TypeA) returns (TagA) {
        match (x ) { case TypeA(w) { return TagA(w); } }
    }
}

enum TypeB { TypeB(word) }
impl Tag<TypeB, TagB> {
    function getTag(x:TypeB) returns (TagB) {
        match (x ) { case TypeB(w) { return TagB(w); } }
    }
}

function tagFirst<a, b, rep1, rep2>(x:a, y:b) returns (rep1)  where a: Tag<rep1>, b: Tag<rep2> {
    return Tag.getTag(x);
}

contract C {
    constructor() {}

    function main() public returns (word) {
        let r : TagA = tagFirst(TypeA(42), TypeB(7));
        match (r ) { case TagA(w) { return w; } }
    }
}
