// Deeply nested synonyms (synonym of synonym of synonym)
type Word1 = word;
type Word2 = Word1;
type Word3 = Word2;

type Pair1 = pair(word, word);
type Pair2 = Pair1;
type Pair3 = Pair2;

function useWord3(x: Word3) -> word {
    return x;
}

function usePair3(p: Pair3) -> word {
    match p {
        | pair(x, _) => return x;
    }
}

function main() -> word {
    let x: Word3 = 42;
    let p: Pair3 = pair(1, 2);
    return useWord3(x);
}
