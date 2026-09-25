enum memory<a> { memory(word) }

function g() returns (unit) {
    let x : memory<memory<word>>;
    let y : memory<word> = memory(1);
    x = memory(0);
}
