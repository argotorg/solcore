import cycleB;
export { fromCycleA };
export cycleB.{fromCycleB};

function fromCycleA() returns (word) {
  return cycleB.fromCycleB();
}
