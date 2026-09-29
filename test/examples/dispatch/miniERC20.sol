import * from std;
import * from std.dispatch;

contract MiniERC20 {
  name : string;
  symbol : string;
  owner : address;
  decimals : uint256; // should be uint8 when we get to it
  totalSupply : uint256;
  balances : mapping(address => uint256);
  allowance : mapping(address => mapping(address => uint256));

  constructor(name_ : memory<string>, symbol_ : memory<string>, totalSupply_:uint256) {
    name = name_;
    symbol = symbol_;
    owner = msgSender();
    decimals = 18;
    mint(totalSupply_);
  }

  function name() public returns (memory<string>) {
    return name;
  }

  function symbol() public returns (memory<string>) {
    return symbol;
  }

  function decimals() public returns (uint256) {
    return decimals;
  }

  function allowance(owner_ : address, spender: address) public returns (uint256) {
    return allowance[owner_][spender]; // don't use "owner" here
  }

  function balanceOf(account : address) public returns (uint256) {
    return balances[account];
  }

  function totalSupply() public returns (uint256) {
    return totalSupply;
  }

  // Note that this is not access guarded — the minting always goes to the owner
  function mint(amount:uint256) public returns (()) {
    balances[owner] = Num.add(balances[owner], amount);
    totalSupply = Num.add(totalSupply, amount);
  }

  function transfer(dst : address, amt : uint256) public returns (bool) {
      return transferFrom(msgSender(), dst, amt);
  }

  function transferFrom(src:address, dst:address, amt:uint256) public returns (bool) {
     let msg_sender = msgSender();
     require(balances[src] >= amt, "transferFrom: insufficient balance");

     if (src != msg_sender && allowance[src][msg_sender] != ((lam (syntaxValue: uint256) -> uint256 { return syntaxValue; })(Num.maxVal()))) {
        require(allowance[src][msg_sender] >= amt, "transferFrom: insufficient allowance");
        allowance[src][msg_sender] -= amt;
     }
     balances[src] = balances[src] - amt;
     balances[dst] = balances[dst] + amt;
     // emit Transfer(src, dst, amt);
     return true;
  }

  function approve(usr: address, amt: uint256) public returns (bool) {
      let msg_sender = msgSender();
      allowance[msg_sender][usr] = amt;
      // emit Approval(msg.sender, usr, amt);
      return true;
  }


  // testing
  function getMyBalance() public returns (uint256) {
    return balances[msgSender()];
  }

  function test() public returns (uint256) {
      approve(address(0), 10);
      transferFrom(msgSender(), address(0), 958);
      return getMyBalance();
  }

}
