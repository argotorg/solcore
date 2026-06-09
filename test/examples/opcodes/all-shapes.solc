import std.opcodes.{*};

// Compilation test for the std/opcodes wrappers.
// Picks two opcodes from each of the four shape categories so the
// pipeline exercises every wrapper signature.

// no inputs, no return
function shape_void_void() -> () {
    stop();
    invalid();
}

// no inputs, returns a word
function shape_void_word() -> word {
    let a = address();
    let t = timestamp();
    return a;
}

// inputs, no return
function shape_word_void(x: word) -> () {
    pop(x);
    mstore(0, x);
}

// inputs, returns a word
function shape_word_word(a: word, b: word) -> word {
    let s = add(a, b);
    let m = mload(0);
    return s;
}
