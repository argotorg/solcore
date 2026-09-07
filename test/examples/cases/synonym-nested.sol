// Deeply nested synonyms (synonym of synonym of synonym)
alias Word1 = word;
alias Word2 = Word1;
alias Word3 = Word2;

alias Pair1 = pair<word, word>;
alias Pair2 = Pair1;
alias Pair3 = Pair2;

function useWord3(x: Word3) returns (word) {
    return x;
}

function usePair3(p: Pair3) returns (word) {
    match (p ) {
        case pair(x, _) { return x;
    } }
}

function main() returns (word) {
    let x: Word3 = 42;
    let p: Pair3 = pair(1, 2);
    return useWord3(x);
}
