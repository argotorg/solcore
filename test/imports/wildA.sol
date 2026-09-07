import wildB;
export {wildB.*, *};

function fromWildA() returns (word) {
  return wildB.fromWildB();
}
