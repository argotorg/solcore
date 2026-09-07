import cycleA;
export { fromCycleB };
export cycleA.{fromCycleA};

function fromCycleB() returns (word) {
  return 2;
}
