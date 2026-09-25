enum Memory<a> { Memory(word) }

function g() returns (unit) {
    let x : Memory<Memory<word>> = Memory(0);
}
