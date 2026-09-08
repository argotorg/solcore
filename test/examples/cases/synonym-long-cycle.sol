// Longer recursive cycle should be rejected
type A = B;
type B = C;
type C = A;

function main() returns (word) {
    return 0;
}
