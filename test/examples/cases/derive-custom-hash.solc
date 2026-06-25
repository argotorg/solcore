import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

forall a.
class a : Hash {
    function hash(x : a) -> word;
}

instance word : Hash {
    function hash(x : word) -> word { return x; }
}

instance () : Hash {
    function hash(x : ()) -> word { return 0; }
}

forall f g . f:Hash, g:Hash =>
instance sum(f, g) : Hash {
    function hash(x : sum(f, g)) -> word {
        match x {
        | inl(u) => return Hash.hash(u);
        | inr(v) => return Hash.hash(v) + 1;
        }
    }
}

forall f g . f:Hash, g:Hash =>
instance (f, g) : Hash {
    function hash(x : (f, g)) -> word {
        match x {
        | (u, v) => return Hash.hash(u) * 31 + Hash.hash(v);
        }
    }
}

#[derive(Hash)]
data Color = Red | Green | Blue;

#[derive(Hash)]
data Pair(a, b) = Pair(a, b);

function hashRed() -> word {
    return Hash.hash(Color.Red);
}

function hashPair() -> word {
    let p : Pair(word, word) = Pair(3, 7);
    return Hash.hash(p);
}
