contract Modifiers {
  constructor() {}

  function ping() public {}

  function deposit() public payable returns (uint256) {
    return 0;
  }

  fallback() payable {}
}
