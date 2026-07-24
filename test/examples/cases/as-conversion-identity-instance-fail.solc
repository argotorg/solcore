trait Typedef<abstract, representation> {
    function abs(value: representation) returns (abstract);
    function rep(value: abstract) returns (representation);
}

impl<t> Typedef<t, t> {
    function abs(value: t) returns (t) {
        return value;
    }

    function rep(value: t) returns (t) {
        return value;
    }
}

enum Wrapped {
    Wrapped(word)
}

function invalid(value: word) returns (Wrapped) {
    return value as Wrapped;
}
