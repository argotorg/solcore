contract Sstore {
  function main() public {
    let res : word;
    assembly {
	sstore(0, 42)
	res := sload(0)
    }
    return res;
  }
}