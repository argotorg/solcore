enum Memory<a> { Memory(word) }

function g() returns (()) {
    let x : Memory<Memory<word>> = Memory(0);
}
