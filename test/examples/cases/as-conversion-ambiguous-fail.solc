trait Typedef<abstract, representation> {
    function abs(value: representation) returns (abstract);
    function rep(value: abstract) returns (representation);
}

enum Left {
    Left(word)
}

enum Right {
    Right(word)
}

impl Typedef<Left, Right> {
    function abs(value: Right) returns (Left) {
        return Left(0);
    }

    function rep(value: Left) returns (Right) {
        return Right(0);
    }
}

impl Typedef<Right, Left> {
    function abs(value: Left) returns (Right) {
        return Right(0);
    }

    function rep(value: Right) returns (Left) {
        return Left(0);
    }
}

function invalid(value: Left) returns (Right) {
    return value as Right;
}
